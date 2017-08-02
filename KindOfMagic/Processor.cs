using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;

namespace KindOfMagic
{
    internal enum LogLevel
    { Verbose, Error, Message, Warning }

    internal delegate void LogDelegate(LogLevel level, string format, params object[] args);

    internal enum MagicResult
    {
        /// <summary>
        /// No magic attributes found, no properties processed, no changes
        /// </summary>
        NoMagicFound,
        /// <summary>
        /// Magic attributes found, no properties processed, assembly labeled
        /// </summary>
        NoMagicNeeded,
        /// <summary>
        /// Magic attributes found, some properties processed, assembly labeled
        /// </summary>
        MagicApplied,
        /// <summary>
        /// Assembly is already processed, no changes
        /// </summary>
        MagicAlreadyApplied
    }

    /// <summary>
    /// Only classes which implement INotifyPropertyChanged are inspected
    /// </summary>
    internal class Processor
    {
        private readonly ModuleDefinition _module;
        private readonly HashSet<string> _magic = new HashSet<string>();
        private readonly HashSet<string> _noMagic = new HashSet<string>();
        private readonly IAssemblyResolver _resolver;

        public Processor(IAssemblyResolver resolver, ModuleDefinition module)
        {
            _resolver = resolver;
            _module = module;
        }

        public LogDelegate Logger;

        #region BeaconMethodName Property

        private string _beaconMethodName;
        public string BeaconMethodName
        {
            get { return _beaconMethodName ?? "Raise"; }
            set { _beaconMethodName = value; }
        }

        #endregion BeaconMethodName Property

        #region RaiseMethodName Property

        private string _raiseMethod;
        public string RaiseMethodName
        {
            get { return _raiseMethod ?? "RaisePropertyChanged"; }
            set { _raiseMethod = value; }
        }

        #endregion RaiseMethodName Property

        #region MagicAttributeName Property

        private string _magicName;
        public string MagicAttributeName
        {
            get { return _magicName ?? "MagicAttribute"; }
            set { _magicName = value; }
        }

        #endregion MagicAttributeName Property

        #region NoMagicAttributeName Property

        private string _noMagicName;
        public string NoMagicAttributeName
        {
            get { return _noMagicName ?? "NoMagicAttribute"; }
            set { _noMagicName = value; }
        }

        #endregion NoMagicAttributeName Property

        private void LogWarning(string format, params object[] args)
        {
            Logger?.Invoke(LogLevel.Warning, format, args);
        }

        private void LogError(string format, params object[] args)
        {
            Logger?.Invoke(LogLevel.Error, format, args);
        }

        private void LogMessage(string format, params object[] args)
        {
            Logger?.Invoke(LogLevel.Message, format, args);
        }

        private void LogVerbose(string format, params object[] args)
        {
            Logger?.Invoke(LogLevel.Verbose, format, args);
        }

        #region Processing Label logic

        private bool? _needsProcessing;
        public bool NeedProcessing
        {
            get
            {
                return _needsProcessing ?? (bool)(_needsProcessing = !GetAlreadyProcessed());
            }
        }

        private const string T_GeneratedCodeAttribute = "System.CodeDom.Compiler.GeneratedCodeAttribute";
        private const string T_AssemblyMetadataAttribute = "System.Reflection.AssemblyMetadataAttribute";

        /// <summary>
        /// Checks main module for label attribute
        /// </summary>
        /// <returns></returns>
        private bool GetAlreadyProcessed()
        {
            return _module.CustomAttributes.Any(i => (i.AttributeType.FullName == T_GeneratedCodeAttribute || i.AttributeType.FullName == T_AssemblyMetadataAttribute) && i.HasConstructorArguments && Convert.ToString(i.ConstructorArguments[0].Value) == "KindOfMagic");
        }

        /// <summary>
        /// Marks main module with label attribute
        /// </summary>
        private void MarkAsProcessed()
        {
            _module.CustomAttributes.Add(CreateLabel());
        }

        /// <summary>
        /// Creates a label class used to mark module as processed
        /// </summary>
        private CustomAttribute CreateLabel()
        {
            var winRT = _module.AssemblyReferences.Any(i => i.Name == "Windows"); // winrt

            var stringType = _module.TypeSystem.String;
            using (var system = GetAssemblyRef(winRT ? "mscorlib" : "System"))
            {
                var codeAttr = winRT ? T_AssemblyMetadataAttribute : T_GeneratedCodeAttribute;

                var baseAttr = system.MainModule.GetType(codeAttr);
                if (baseAttr == null)
                {
                    var attr = system.MainModule.ExportedTypes.Single(i => i.FullName == codeAttr);
                    baseAttr = attr.Resolve();
                }

                var ctor = baseAttr.Methods.First(m => m.IsConstructor && m.Parameters.Count == 2);
                var baseCtor = _module.ImportReference(ctor);

                var result = new CustomAttribute(baseCtor);

                result.ConstructorArguments.Add(new CustomAttributeArgument(stringType, "KindOfMagic"));
                result.ConstructorArguments.Add(new CustomAttributeArgument(stringType, "1.0"));

                return result;
            }
        }

        private AssemblyDefinition GetAssemblyRef(string name)
        {
            var corlib = (AssemblyNameReference)_module.TypeSystem.CoreLibrary;

            // Workaround to get System.dll
            if (corlib.PublicKeyToken[0] != 124) // public key differs for SL & NETFX
                return _resolver.Resolve(AssemblyNameReference.Parse(name));

            return _resolver.Resolve(new AssemblyNameReference(name, corlib.Version) { PublicKeyToken = corlib.PublicKeyToken });
        }

        #endregion Processing Label logic

        private static readonly string[] _ignores = new[] { "mscorlib,", "System,", "System.", "Microsoft.", "DevExpress.", "Windows,", "WindowsBase,", "PresentationCore,", "PresentationFramework,", };

        /// <summary>
        /// Small filter method to discard not interesting assemblies. By default System, MS & DevEx.
        /// </summary>
        private bool IsUserAssembly(AssemblyNameReference assembly)
        {
            var name = assembly.FullName;

            return !_ignores.Any(i => name.StartsWith(i, StringComparison.OrdinalIgnoreCase));
        }

        private bool FindAnyMagic()
        {
            FindMagic(_module);

            return _magic.Count > 0;
        }

        private HashSet<string> _processed = new HashSet<string>();

        private void FindMagic(ModuleDefinition module)
        {
            if (_processed.Contains(module.Assembly.FullName))
                return;

            _processed.Add(module.Assembly.FullName);

            FindMagicInModule(module);

            foreach (var reference in module.AssemblyReferences)
                if (IsUserAssembly(reference))
                    try
                    {
                        var assembly = _resolver.Resolve(reference);

                        if (assembly != null)
                            FindMagic(assembly.MainModule);
                    }
                    catch (Exception)
                    {
                        // TODO: Log exception
                    }
        }

        private void FindMagicInModule(ModuleDefinition module)
        {
            _magic.UnionWith(AttributesWithName(module, MagicAttributeName));
            _noMagic.UnionWith(AttributesWithName(module, NoMagicAttributeName));
        }

        private static IEnumerable<string> AttributesWithName(ModuleDefinition module, string name)
        {
            return from t in module.Types
                   where t.IsClass && !t.IsAbstract && t.Name == name && t.BaseType != null && t.BaseType.FullName == "System.Attribute"
                   select t.FullName;
        }

        /// <summary>
        /// Processes module
        /// </summary>
        /// <returns>Returns null is module is already processed.
        /// Returns true is some properties are transformed, false otherwise.
        /// </returns>
        public MagicResult Process()
        {
            if (!NeedProcessing)
                return MagicResult.MagicAlreadyApplied;

            if (!FindAnyMagic())
                return MagicResult.NoMagicFound;

            int processed = 0;
            var inpc = typeof(INotifyPropertyChanged).FullName;

            try
            {
                var assemblyLevel = _module.Assembly.CustomAttributes.ContainsAttributeFrom(_magic);

                // making a list of classes to process
                var jobs = (from type in GetClasses(_module.Types)
                                // which have proper Raise method
                            let raise = FindRaiseMethodOf(type, inpc)
                            where raise != null
                            // select job related data
                            select new { Type = type, Raise = ImportMethod(type, raise), IsRaiseVirtual = raise.IsVirtual, ClassLevel = FindImplicit(type, assemblyLevel) });

                // making a list of properties to process
                var tasks = from job in jobs
                            from prop in job.Type.Properties
                                // must have public getter and any setter
                            where prop.GetMethod != null && !prop.GetMethod.IsStatic && prop.GetMethod.IsPublic && prop.SetMethod != null
                            // must not have NoMagic attribute applied
                            where !prop.CustomAttributes.ContainsAttributeFrom(_noMagic)
                            // must have assembly, class or property level Magic attribute applied
                            where job.ClassLevel || prop.CustomAttributes.ContainsAttributeFrom(_magic)
                            // select task related data
                            select new { Job = job, Property = prop };

                foreach (var task in tasks)
                {
                    ProcessProperty(task.Property, task.Job.Raise, task.Job.IsRaiseVirtual);
                    processed++;
                }
            }
            finally
            {
                MarkAsProcessed();
            }

            return processed > 0 ? MagicResult.MagicApplied : MagicResult.NoMagicNeeded;
        }

        private IEnumerable<TypeDefinition> GetClasses(IEnumerable<TypeDefinition> types)
        {
            foreach (var i in types)
                if (i.IsClass)
                {
                    yield return i;

                    foreach (var j in GetClasses(i.NestedTypes))
                        yield return j;
                }
        }

        private MethodDefinition FindRaiseMethodOf(TypeDefinition type, string intf)
        {
            // we scan class hierarchy to find INPC interface implementation
            return (from t in GetClassHierarchy(type)
                    where t.Interfaces.Any(i => i.InterfaceType.FullName == intf)
                    // and have proper and accessible RaisePropertyChanged
                    let raise = FindRaiseMethod(t)
                    where raise != null && (t == type || !raise.IsPrivate)
                    select raise).FirstOrDefault();
        }

        private MethodReference ImportMethod(TypeDefinition type, MethodReference method)
        {
            var original = method;

            if (method != null && type.HasGenericParameters)
            {
                var genericType = type.MakeGenericInstanceType(type.GenericParameters.ToArray());
                var genericMethod = new MethodReference(method.Name, method.ReturnType, genericType)
                {
                    CallingConvention = method.CallingConvention,
                    HasThis = method.HasThis
                };
                if (method.HasParameters)
                    foreach (var i in method.Parameters)
                        genericMethod.Parameters.Add(i);

                method = genericMethod;
            }

            return _module.ImportReference(method);
        }

        private MethodDefinition FindRaiseMethod(TypeDefinition type)
        {
            var name = RaiseMethodName;
            var methods = from m in type.Methods
                          where !m.IsStatic && m.Name == name
                          && m.ReturnType.FullName == "System.Void"
                          && m.HasParameters && m.Parameters.Count == 1 && m.Parameters[0].ParameterType.FullName == "System.String"
                          select m;

            return methods.FirstOrDefault();
        }

        /// <summary>
        /// We traverse class hierarchy upwards and look for Magic/NoMagic attribute.
        /// If Magic attribute found first, class transformation is implicit.
        /// Otherwise explicit.
        /// </summary>
        private bool FindImplicit(TypeDefinition type, bool @default)
        {
            var x = (from t in GetClassHierarchy(type)
                     from a in t.CustomAttributes
                     let at = a.AttributeType.FullName
                     select new
                     {
                         NoMagic = _noMagic.Contains(at),
                         Magic = _magic.Contains(at)
                     }).FirstOrDefault(i => i.NoMagic || i.Magic);

            return x == null ? @default : x.Magic;
        }

        private void ProcessProperty(PropertyDefinition p, MethodReference raise, bool isVirtual)
        {
            if (p.SetMethod.Body == null) return;

            LogVerbose("Enchanting {0}.{1}...", p.DeclaringType.FullName, p.Name);

            var ops = new Instructions();
            var code = p.SetMethod.Body.Instructions;
            var pt = p.PropertyType;
            var startIndex = 0;
            var ret = code.Last();

            if (pt.IsGenericParameter)
            {
                LogVerbose("No spell for equality check of generic properties yet.");
                // TODO:
            }
            else
            {
                var field = DetectVar(p.GetMethod);
                var nt = GetUnderlyingType(pt);

                // special handling of Nullable<T>
                if (nt != null)
                {
                    var ptd = pt.Resolve();

                    var hasValue = MakeGeneric(ptd.Properties.Single(i => i.Name == "HasValue").GetMethod, pt);
                    var getValue = MakeGeneric(ptd.Methods.Single(i => !i.IsStatic && i.Name == "GetValueOrDefault" && !i.HasParameters), pt);
                    var useEqual = !(nt.IsPrimitive || nt.FullName == "System.Single");  // Mono.Cecil bug workaround

                    var equal = useEqual ? GetEqualityMethod(nt) : null;
                    var instanceEqual = equal != null && equal.Parameters.Count == 1;

                    var value = p.SetMethod.Parameters[0];
                    var vd1 = new VariableDefinition(pt);
                    p.SetMethod.Body.InitLocals = true;
                    p.SetMethod.Body.Variables.Add(vd1);

                    var end = code.First();

                    #region var vd1 = this.Field

                    ops.Ldarg_0();

                    if (field != null)
                        ops.Ldfld(field);
                    else
                        ops.Call(ImportMethod(p.DeclaringType, p.GetMethod));

                    ops.Stloc_S(vd1);

                    #endregion var vd1 = this.Field

                    if (SizeOf(nt) <= 4)
                    {
                        #region if (vd1.GetValueOrDefault() != value.GetValueOrDefault()) goto end;

                        ops.Ldloca_S(vd1);
                        ops.Call(getValue);
                        ops.Ldarga_S(value);
                        ops.Call(getValue);

                        if (equal == null)
                            ops.Bne_Un_S(end);
                        else
                        {
                            if (equal != null && equal.Parameters.Count == 1) // x.Equals(y)
                            {
                                var vd2 = new VariableDefinition(nt);
                                p.SetMethod.Body.Variables.Add(vd2);

                                ops.Stloc_S(vd2);
                                ops.Ldloca_S(vd2);
                            }

                            ops.Call(equal);
                            ops.Brfalse_S(end);
                        }

                        #endregion if (vd1.GetValueOrDefault() != value.GetValueOrDefault()) goto end;

                        #region if (vd1.HasValue == value.HasValue) return;

                        ops.Ldloca_S(vd1);
                        ops.Call(hasValue);
                        ops.Ldarga_S(value);
                        ops.Call(hasValue);
                        ops.Beq_S(ret);

                        #endregion if (vd1.HasValue == value.HasValue) return;
                    }
                    else
                    {
                        #region HasValue

                        var varHasValue = new VariableDefinition(hasValue.ReturnType);
                        p.SetMethod.Body.Variables.Add(varHasValue);

                        #endregion HasValue

                        #region if (vd1.HasValue != value.HasValue) goto end;

                        ops.Ldloca_S(vd1);
                        ops.Call(hasValue);
                        ops.Ldarga_S(value);
                        ops.Call(hasValue);
                        ops.Stloc_S(varHasValue);
                        ops.Ldloc(varHasValue);
                        ops.Bne_Un_S(end);

                        #endregion if (vd1.HasValue != value.HasValue) goto end;

                        #region if (!value.HasValue) return;

                        ops.Ldloc(varHasValue);
                        ops.Brfalse_S(ret);

                        #endregion if (!value.HasValue) return;

                        #region if (vd1.GetValueOrDefault() == value.GetValueOrDefault()) return;

                        ops.Ldloca_S(vd1);
                        ops.Call(getValue);

                        if (instanceEqual) // x.Equals(y)
                        {
                            var vd2 = new VariableDefinition(nt);
                            p.SetMethod.Body.Variables.Add(vd2);

                            ops.Stloc_S(vd2);
                            ops.Ldloca_S(vd2);
                        }

                        ops.Ldarga_S(value);
                        ops.Call(getValue);

                        if (equal == null)
                            ops.Beq_S(ret);
                        else
                        {
                            ops.Call(equal);
                            ops.Brtrue_S(ret);
                        }

                        #endregion if (vd1.GetValueOrDefault() == value.GetValueOrDefault()) return;
                    }
                }
                else
                {
                    var useEqual = !pt.IsArray && !pt.IsPrimitive;
                    var equal = useEqual ? GetEqualityMethod(pt) : null;
                    var instanceEqual = equal != null && equal.Parameters.Count == 1;
                    var vd1 = default(VariableDefinition);

                    var valueType = pt.IsValueType;
                    if (valueType)
                    {
                        vd1 = new VariableDefinition(pt);
                        p.SetMethod.Body.InitLocals = true;
                        p.SetMethod.Body.Variables.Add(vd1);
                    }

                    #region if (this.field == value) return;

                    ops.Ldarg_0();

                    if (field != null)
                        ops.Ldfld(field);
                    else
                        ops.Call(ImportMethod(p.DeclaringType, p.GetMethod));

                    if (valueType && instanceEqual)
                    {
                        ops.Stloc_S(vd1);
                        ops.Ldloca_S(vd1);
                    }

                    ops.Ldarg_1();

                    if (equal == null)
                        ops.Beq(ret);
                    else
                    {
                        ops.Call(equal);
                        ops.Brtrue_S(ret);
                    }

                    #endregion if (this.field == value) return;
                }

                ops.InsertInto(code);

                startIndex = ops.Count;

                ops.Clear();
            }

            #region this.RaisePropertyChanged(p.Name);

            ops.Ldarg_0();
            ops.Ldstr(p.Name);
            if (isVirtual)
                ops.Callvirt(raise);
            else
                ops.Call(raise);

            #endregion this.RaisePropertyChanged(p.Name);

            var retIndex = code.Count - 1;
            var beaconFound = false;

            for (var i = retIndex; i > startIndex; i--)
            {
                var op = code[i];
                if (op.OpCode == OpCodes.Call && IsBeacon(op.Operand as MethodReference))
                {
                    beaconFound = true;
                    ops.MergeInto(code, i);
                }
            }

            if (!beaconFound)
            {
                ops.InsertInto(code, retIndex);
                // otherwise, remap all ret jumps to RaisePropertyChanged call

                var hasRetJumps = false;
                var newRet = code[retIndex];

                for (var i = startIndex; i < retIndex; i++)
                {
                    var op = code[i];
                    if ((op.OpCode == OpCodes.Brtrue_S || op.OpCode == OpCodes.Brtrue
                      || op.OpCode == OpCodes.Brfalse_S || op.OpCode == OpCodes.Brfalse
                      || op.OpCode == OpCodes.Br || op.OpCode == OpCodes.Br_S)
                      && op.Operand == ret)
                    {
                        if (i < retIndex - 1)
                            hasRetJumps = true;

                        op.Operand = newRet;
                    }
                }

                if (hasRetJumps)
                    LogWarning("Property {0}.{1} setter is too complex. Use beacon method (static void {2}()) to indicate point of {3} injection.", p.DeclaringType.FullName, p.Name, BeaconMethodName, RaiseMethodName);
            }
        }

        private bool IsBeacon(MethodReference method)
        {
            return method != null && !method.HasThis && !method.HasParameters && method.ReturnType.FullName == "System.Void" && method.Name == BeaconMethodName;
        }

        private static IEnumerable<TypeDefinition> GetClassHierarchy(TypeDefinition type, bool includeSystem = false)
        {
            for (var scan = type; scan != null; scan = Resolve(scan.BaseType, includeSystem))
                yield return scan;
        }

        private static TypeDefinition FindInterfaceImplementation(TypeDefinition type, string name)
        {
            return GetClassHierarchy(type).FirstOrDefault(t => t.Interfaces.Any(i => i.InterfaceType.FullName == name));
        }

        private static TypeDefinition Resolve(TypeReference reference, bool includeSystem = true)
        {
            if (!includeSystem && reference != null && (reference.FullName.StartsWith("System.") || reference.FullName.StartsWith("Windows.")))
                return null;

            return reference?.Resolve();
        }

        private MethodReference MakeGeneric(MethodReference method, TypeReference declaringType)
        {
            var reference = new MethodReference(method.Name, method.ReturnType, declaringType)
            {
                HasThis = method.HasThis,
                CallingConvention = method.CallingConvention
            };
            foreach (var parameter in method.Parameters)
                reference.Parameters.Add(new ParameterDefinition(parameter.ParameterType));

            return _module.ImportReference(reference);
        }

        private MethodReference GetEqualityMethod(TypeReference reference)
        {
            var type = reference.Resolve();
            if (type == null)
                return null;

            {
                var result = type.Methods.SingleOrDefault(i => i.IsStatic &&
                                         i.Name == "op_Equality" &&
                                         i.HasParameters &&
                                         i.Parameters.Count == 2 &&
                                         i.Parameters[0].ParameterType == type &&
                                         i.Parameters[1].ParameterType == type);

                if (result != null)
                    return _module.ImportReference(result);
            }

            if (type.IsValueType)
            {
                var result = type.Methods.SingleOrDefault(i => !i.IsStatic &&
                                         i.Name == "Equals" &&
                                         i.HasParameters &&
                                         i.Parameters.Count == 1 &&
                                         i.Parameters[0].ParameterType == type);

                if (result != null)
                    return _module.ImportReference(result);
            }

            return null;
        }

        /// <summary>
        /// Determing the property field, by looking into first Get method.
        /// Works only for simple getters: return field;
        /// </summary>
        private static FieldReference DetectVar(MethodDefinition method)
        {
            if (method == null || method.Body == null)
                return null;

            var instructions = method.Body.Instructions;

            return DetectVarSimple(instructions) ?? DetectVarSimple2(instructions) ?? DetectVarLazy(instructions);
        }

        private static FieldReference DetectVarSimple(ICollection<Instruction> instructions)
        {
            var ops = instructions.Reverse().Where(i => i.OpCode != OpCodes.Nop).Take(3).ToList();

            if (ops.Count != 3)
                return null;

            if (ops[0].OpCode != OpCodes.Ret)
                return null;

            if (ops[1].OpCode != OpCodes.Ldfld)
                return null;

            if (ops[2].OpCode != OpCodes.Ldarg_0)
                return null;

            return ops[1].Operand as FieldReference;
        }

        private static FieldReference DetectVarSimple2(ICollection<Instruction> instructions)
        {
            var ops = instructions.Reverse().Where(i => i.OpCode != OpCodes.Nop).Take(6).ToList();

            if (ops.Count != 6)
                return null;

            if (ops[0].OpCode != OpCodes.Ret)
                return null;

            if (ops[1].OpCode != OpCodes.Ldloc_0)
                return null;

            if (ops[2].OpCode != OpCodes.Br_S)
                return null;

            if (ops[3].OpCode != OpCodes.Stloc_0)
                return null;

            if (ops[4].OpCode != OpCodes.Ldfld)
                return null;

            if (ops[5].OpCode != OpCodes.Ldarg_0)
                return null;

            return ops[4].Operand as FieldReference;
        }

        private static FieldReference DetectVarLazy(ICollection<Instruction> instructions)
        {
            var ops = instructions.Where(i => i.OpCode != OpCodes.Nop).Take(5).ToList();

            if (ops.Count < 5)
                return null;

            if (ops[0].OpCode != OpCodes.Ldarg_0)
                return null;

            if (ops[1].OpCode != OpCodes.Ldfld)
                return null;

            if (ops[2].OpCode != OpCodes.Dup)
                return null;

            if (ops[3].OpCode != OpCodes.Brtrue_S && ops[3].OpCode != OpCodes.Brtrue)
                return null;

            if (ops[4].OpCode != OpCodes.Pop)
                return null;

            return ops[1].Operand as FieldReference;
        }

        /// <summary>
        /// Hack to determine NullableOfT
        /// </summary>
        private static TypeDefinition GetUnderlyingType(TypeReference type)
        {
            if (type is GenericInstanceType git && git.Name.StartsWith("Nullable`"))
                return git.GenericArguments[0].Resolve();

            return null;
        }

        private Dictionary<TypeReference, int> _sizeCache = new Dictionary<TypeReference, int>();

        /// <summary>
        /// Hack to determine size of a struct/value type
        /// </summary>
        private int SizeOf(TypeReference type)
        {
            if (_sizeCache.TryGetValue(type, out int result))
                return result;

            result = 4;

            var td = type.Resolve();
            if (td.IsEnum)
                result = SizeOf(td.GetEnumUnderlyingType());
            else
              if (td.IsValueType)
            {
                switch (td.FullName)
                {
                    case "System.Boolean":
                    case "System.Byte":
                    case "System.SByte":
                        result = sizeof(byte);
                        break;

                    case "System.Char":
                    case "System.Int16":
                    case "System.UInt16":
                        result = sizeof(char);
                        break;

                    case "System.Int32":
                    case "System.UInt32":
                        result = sizeof(int);
                        break;

                    case "System.Single":
                        result = sizeof(float);
                        break;

                    case "System.Decimal":
                        result = sizeof(decimal);
                        break;

                    case "System.Double":
                    case "System.Int64":
                    case "System.UInt64":
                    case "System.DateTime":
                    case "System.TimeSpan":
                    case "System.Currency":
                        result = sizeof(long);
                        break;

                    case "System.DateTimeOffset":
                        result = 10;
                        break;

                    case "System.Guid":
                        result = 32;
                        break;

                    case "System.IntPtr":
                    case "System.UIntPtr":
                        result = IntPtr.Size;
                        break;

                    default:
                        result = td.Fields.Where(i => !i.IsStatic).Sum(i => SizeOf(i.FieldType));
                        break;
                }
            }

            _sizeCache[type] = result;

            return result;
        }
    }

    internal static class Extensions
    {
        public static bool ContainsAttributeFrom(this IEnumerable<CustomAttribute> source, HashSet<string> attributes)
        {
            return source.Any(a => attributes.Contains(a.AttributeType.FullName));
        }
    }
}
