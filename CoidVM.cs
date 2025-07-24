using System;

namespace CoidVM
{
    public enum EnvError
    {
        NO_ERROR,
        UNKNOWN_TOKEN,
        UNKNOWN_LEXER_STATE,
        UNKNOWN_PARSER_STATE,
        UNKNOWN_PCODE,
        UNDEFINED_SYMBOL,
        UNDEFINED_LABEL,
        REGISTER_OVERFLOW,
        CONSTANT_OVERFLOW,
        FUNCTION_OVERFLOW,
        LABEL_OVERFLOW,
        JUMP_OUT_OF_RANGE,
        BREAK_OUTSIDE_LOOP,
        CONTINUE_OUTSIDE_LOOP,
        INVALID_TYPE
    }

    public enum StateError
    {
        NO_ERROR,
        UNDEFINED_FUNCTION,
        CALLSTACK_OVERFLOW,
        DATASTACK_OVERFLOW,
        UNKNOWN_INSTRUCTION,
    }

    public enum InstOp : byte
    {
        ADD,
        SUB,
        MUL,
        DIV,
        SLT,
        SGE,
        SEQ,
        SNE,
        JNZ,
        JZR,
        JINC,
        JDEC,
        CALL,
        RET,
        LDC,
        LDI
    }

    public struct Inst
    {
        public InstOp op; 
        public byte a, b, c;
    }

    public struct FuncInfo
    {
        public int regSize;
        public int codeAddr;
    }

    public enum SymbolType
    {
        UNDEFINED,
        VAR,
        FUNCTION,
        CONSTANT,
        IMMEDIATE,
        LABEL
    }
    public struct SymbolInfo
    {
        public SymbolType symType;
        public int index;
    }

    public class CoidEnv
    {
        enum TokenType
        {
            WS,
            EOF,
            IDENT,
            FUNCT,
            LABEL,
            INTEGER,
            FLOAT,
            STRING,
            OPERATOR
        }

        enum LexerState
        {
            WS,
            IDENT,
            INTEGER,
            FLOAT,
            STRING,
            ESCAPE,
            OPERATOR,
            COMMENT
        }

        enum ParserState
        {
            PROGRAM1,
            USING1, USING2, USING3,
            FUNCTION1, FUNCTION2, FUNCTION3, FUNCTION4, FUNCTION5,
            BLOCK1,
            ASSIGN1, ASSIGN2,
            VAR1,
            IF1, IF2, IF3, IF4,
            WHILE1, WHILE2,
            FOR1, FOR2, FOR3, FOR4,
            RETURN1, RETURN2,
            GOTO1,
            CALL1, CALL2,
            ATOM1, ATOM2, ATOM3, ATOM4,
            EXPR1, EXPR2
        }

        struct ParserCallInfo
        {
            public ParserState state;
            public int data1;
            public int data2;
            public int data3;
        }

        public static int Main()
        {
            string? input = Console.ReadLine();
            string s = input != null ? input : " ";
            Console.WriteLine("你输入的字符串是: " + input);
            CoidEnv env = new CoidEnv();
            env.NewInput("Console");
            //Console.WriteLine("env.tokenType: " + env.tokenType + " env.lexerCache: " + env.lexerCache + " env.lexerState: " + env.lexerState);
            foreach (char c in s)
            {
                env.InputChar(c);
                //Console.WriteLine(env.cc + " value: " + (int)env.cc);
                if (env.tokenType != TokenType.WS)
                {
                    Console.WriteLine("env.tokenType: " + env.tokenType + " env.tokenString: " + env.tokenString);
                }
            }
            env.CloseInput();
            if (env.isPanic) Console.WriteLine("发生错误！" + env.envError);
            for (int i = 0; i < env.instList.Count; i++)
            {
                Inst inst = env.instList[i];
                Console.WriteLine((int)i + "\t" + inst.op + "\t" + (int)inst.a + "\t" + (int)inst.b + "\t" + (int)inst.c);
            }
            return 0;
        }
        //////////////////// Variables ////////////////////
        ////////// Runtime Related //////////
        // Program
        public List<Inst> instList = new List<Inst>();
        public List<FuncInfo> funcInfoList = new List<FuncInfo>();
        // Constant
        public List<double> constNumberList = new List<double>();
        public List<string> constStringList = new List<string>();
        Dictionary<double, int> constNumberCache = new Dictionary<double, int>();
        Dictionary<string, int> constStringCache = new Dictionary<string, int>();

        ////////// Compile Related //////////
        public bool isPanic = false;
        public EnvError envError = EnvError.NO_ERROR;
        public string inputName = "";
        public int lineCounter = 0;

        ////////// Lexer Related //////////
        string lexerCache = "";
        LexerState lexerState = LexerState.WS;
        char cc = ' ';  // char cache
        TokenType tokenType = TokenType.WS;
        string tokenString = "";

        ////////// Parser Related //////////
        List<Dictionary<string, SymbolInfo>> symbolDicts = new List<Dictionary<string, SymbolInfo>>();
        List<int> labelList = new List<int>();
        ParserState parserState = ParserState.PROGRAM1;
        int parserData1 = 0;
        int parserData2 = 0;
        int parserData3 = 0;
        int parserDataRet = 0;
        int parserPcodeRet = 0;
        List<ParserCallInfo> parserStack = new List<ParserCallInfo>();
        bool parserStop = true;
        string parserCache = "";
        int regCount = 0;
        int regMaxCount = 0;
        int regTempCount = 0;

        //////////////////// Functions ////////////////////
        public CoidEnv()
        {
            symbolDicts.Add(new Dictionary<string, SymbolInfo>());
            parserState = ParserState.PROGRAM1;
        }

        public void NewInput(in string input_name)
        {
            inputName = input_name;
            lineCounter = 0;
        }

        public void InputChar(char c)
        {
            cc = c;
            LexerScan();
            if (tokenType != TokenType.WS) ParserScan();
        }

        public void CloseInput()
        {
            InputChar('\n');
            tokenType = TokenType.EOF;
            ParserScan();
        }

        public void ClearError()
        {
            isPanic = false;
            envError = EnvError.NO_ERROR;
            lexerCache = "";
            lexerState = LexerState.WS;
            tokenType = TokenType.WS;
            tokenString = "";
            symbolDicts.RemoveRange(1, symbolDicts.Count-1);
            labelList.Clear();
            parserState = ParserState.PROGRAM1;
            parserStack.Clear();
            parserCache = "";
            regCount = 0;
            regMaxCount = 0;
            regTempCount = 0;
        }

        public void Clear()
        {
            ClearError();
            instList.Clear();
            funcInfoList.Clear();
            constNumberList.Clear();
            constNumberCache.Clear();
            constStringList.Clear();
            constStringCache.Clear();
            symbolDicts[0].Clear();
        }

        public void SetSysFunction(int call_code)   // call_code >= 0
        {
            funcInfoList.Add(new FuncInfo { codeAddr = -call_code-1, regSize = 1 });
        }

        public SymbolInfo FindGlobalSymbol(in string symbol_name)
        {
            return FindSymbol(symbol_name, 0, 0);
        }

        ////////// Error Handling //////////
        void SetError(EnvError env_error)
        {
            isPanic = true; 
            envError = env_error;
            parserStop = true;
        }

        ////////// Lexer related //////////
        static bool IsWhiteSpace(char c)
        {
            if (c == ' ' || c == '\t' || c == '\v' || c == '\r' || c == '\n' || c == '\f')
            { 
                return true; 
            }
            else { return false; }
        }
        
        static bool IsLetter(char c)
        {
            if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
            {
                return true;
            }
            else { return false; }
        }

        static bool IsNumber(char c)
        {
            if (c >= '0' && c <= '9')
            {
                return true;
            }
            else { return false; }
        }

        static bool IsOp(char c)
        {
            // Op: ( ) * + , - < = > ! / { }
            if ((c >= '(' && c <= '-') || (c >= '<' && c <= '>')
               || c == '!' || c == '/' || c == '{' || c == '}')
            {
                return true;
            }
            else { return false; }
        }

        void LexerChangeState(LexerState new_state, bool truncate, bool append)
        {
            lexerState = new_state;
            if (truncate) lexerCache = "";
            if (append) lexerCache += cc;
            tokenType = TokenType.WS;
        }

        void LexerOutput(TokenType tk_type, LexerState new_state, bool truncate, bool append)
        {
            tokenString = lexerCache;
            LexerChangeState(new_state, truncate, append);
            tokenType = tk_type;
        }

        void LexerScan()
        {
            if (isPanic) return;
            if (cc == '\n') lineCounter += 1;
            switch (lexerState)
            {
                case LexerState.WS:
                    if (IsWhiteSpace(cc)) LexerChangeState(LexerState.WS, false, false);
                    else if (IsLetter(cc)) LexerChangeState(LexerState.IDENT, true, true);
                    else if (IsNumber(cc)) LexerChangeState(LexerState.INTEGER, true, true);
                    else if (cc == '.') LexerChangeState(LexerState.FLOAT, true, true);
                    else if (cc == '"') LexerChangeState(LexerState.STRING, true, false);
                    else if (IsOp(cc)) LexerChangeState(LexerState.OPERATOR, true, true);
                    else SetError(EnvError.UNKNOWN_TOKEN); break;
                case LexerState.IDENT:
                    if (IsLetter(cc) || IsNumber(cc)) LexerChangeState(LexerState.IDENT, false, true);
                    else if (cc == '(') LexerOutput(TokenType.FUNCT, LexerState.WS, true, false);
                    else if (cc == ':') LexerOutput(TokenType.LABEL, LexerState.WS, true, false);
                    else if (IsOp(cc)) LexerOutput(TokenType.IDENT, LexerState.OPERATOR, true, true);
                    else if (IsWhiteSpace(cc)) LexerOutput(TokenType.IDENT, LexerState.WS, true, false);
                    else SetError(EnvError.UNKNOWN_TOKEN); break;
                case LexerState.INTEGER:
                    if (IsNumber(cc)) LexerChangeState(LexerState.INTEGER, false, true);
                    else if (cc == '.') LexerChangeState(LexerState.FLOAT, false, true);
                    else if (IsOp(cc)) LexerOutput(TokenType.INTEGER, LexerState.OPERATOR, true, true);
                    else if (IsWhiteSpace(cc)) LexerOutput(TokenType.INTEGER, LexerState.WS, true, false);
                    else SetError(EnvError.UNKNOWN_TOKEN); break;
                case LexerState.FLOAT:
                    if (IsNumber(cc)) LexerChangeState(LexerState.FLOAT, false, true);
                    else if (IsOp(cc)) LexerOutput(TokenType.FLOAT, LexerState.OPERATOR, true, true);
                    else if (IsWhiteSpace(cc)) LexerOutput(TokenType.FLOAT, LexerState.WS, true, false);
                    else SetError(EnvError.UNKNOWN_TOKEN); break;
                case LexerState.STRING:
                    if (cc == '"') LexerOutput(TokenType.STRING, LexerState.WS, true, false);
                    else if (cc == '\\') LexerChangeState(LexerState.ESCAPE, false, false);
                    else if (cc != '\n') LexerChangeState(LexerState.STRING, false, true);
                    else SetError(EnvError.UNKNOWN_TOKEN); break;
                case LexerState.ESCAPE:
                    if (cc == 'f') cc = '\f';
                    else if (cc == 'n') cc = '\n';
                    else if (cc == 'r') cc = '\r';
                    else if (cc == 't') cc = '\t';
                    else if (cc == 'v') cc = '\v';
                    LexerChangeState(LexerState.STRING, false, true); break;
                case LexerState.OPERATOR:
                    if (IsNumber(cc)) LexerOutput(TokenType.OPERATOR, LexerState.INTEGER, true, true);
                    else if (IsLetter(cc)) LexerOutput(TokenType.OPERATOR, LexerState.IDENT, true, true);
                    else if (cc == '.') LexerOutput(TokenType.OPERATOR, LexerState.FLOAT, true, true);
                    else if (cc == '"') LexerOutput(TokenType.OPERATOR, LexerState.STRING, true, false);
                    else if (IsWhiteSpace(cc)) LexerOutput(TokenType.OPERATOR, LexerState.WS, true, false);
                    else if (cc == '=') LexerChangeState(LexerState.OPERATOR, false, true);
                    else if (cc == '/' && lexerCache == "/") LexerChangeState(LexerState.COMMENT, false, false);
                    else if (IsOp(cc)) LexerOutput(TokenType.OPERATOR, LexerState.OPERATOR, true, true);
                    else SetError(EnvError.UNKNOWN_TOKEN); break;
                case LexerState.COMMENT:
                    if (cc == '\n') LexerChangeState(LexerState.WS, true, false);
                    else LexerChangeState(LexerState.COMMENT, false, false); break;
                default:
                    SetError(EnvError.UNKNOWN_LEXER_STATE); break;
            }
        }

        ////////// Constant & Symbol related //////////
        SymbolInfo CreateFloat(in string s)
        {
            double t = double.Parse(s);
            if (constNumberCache.ContainsKey(t))
            {
                return new SymbolInfo { symType = SymbolType.CONSTANT, index = constNumberCache[t] };
            }
            else
            {
                constNumberList.Add(t);
                constNumberCache[t] = constNumberList.Count - 1;
                return new SymbolInfo { symType = SymbolType.CONSTANT, index = constNumberList.Count - 1 };
            }
        }

        SymbolInfo CreatePointer(int t)
        {
            if (t >= 0 && t <= 65535)
            {
                return new SymbolInfo { symType = SymbolType.IMMEDIATE, index = t };
            }
            else if (constNumberCache.ContainsKey(t))
            {
                return new SymbolInfo { symType = SymbolType.CONSTANT, index = constNumberCache[t] };
            }
            else
            {
                constNumberList.Add(t);
                constNumberCache[t] = constNumberList.Count - 1;
                return new SymbolInfo { symType = SymbolType.CONSTANT, index = constNumberList.Count - 1 };
            }
        }

        SymbolInfo CreateInteger(in string s)
        {
            return CreatePointer(int.Parse(s));
        }

        SymbolInfo CreateString(in string s)
        {
            if (s.Length < 32)  // short string optimization
            {
                if (constStringCache.ContainsKey(s))
                {
                    return CreatePointer(constStringCache[s]);
                }
                else
                {
                    constStringList.Add(s);
                    constStringCache[s] = constStringList.Count - 1;
                    return CreatePointer(constStringList.Count - 1);
                }
            }
            else
            {
                constStringList.Add(s);
                return CreatePointer(constStringList.Count - 1);
            }
        }

        SymbolInfo FindSymbol(in string sym_name, int start_level, int end_level)
        {
            for (int i = start_level; i >= end_level; i--)
            {
                if (symbolDicts[i].ContainsKey(sym_name))
                {
                    return symbolDicts[i][sym_name];
                }
            }
            return new SymbolInfo { symType = SymbolType.UNDEFINED, index = -1 };
        }

        int CreateFunction(in string s)
        {
            SymbolInfo info = FindSymbol(s, 0, 0);
            if (info.symType == SymbolType.FUNCTION) return info.index;
            else
            {
                funcInfoList.Add(new FuncInfo { codeAddr = 0, regSize = -1 });
                symbolDicts[0][s] = new SymbolInfo
                {
                    symType = SymbolType.FUNCTION,
                    index = funcInfoList.Count - 1
                };
                return funcInfoList.Count - 1;  // funcInfoList index
            }
        }
        int CreateLabel(in string s)
        {
            SymbolInfo info = FindSymbol(s, 1, 1);
            if (info.symType == SymbolType.LABEL) return info.index;
            else
            {
                labelList.Add(-1);
                symbolDicts[1][tokenString + ":"] = new SymbolInfo
                {
                    symType = SymbolType.LABEL,
                    index = labelList.Count - 1
                };
                return labelList.Count - 1;
            }
        }
        int CreateVar(in string s)
        {
            SymbolInfo info = FindSymbol(s, symbolDicts.Count - 1, symbolDicts.Count - 1);
            if (info.symType == SymbolType.VAR) return info.index;
            else
            {
                int idx = RegAlloc();
                symbolDicts[symbolDicts.Count - 1][tokenString] = new SymbolInfo
                {
                    symType = SymbolType.VAR,
                    index = idx
                };
                return idx;     // register index
            }
        }
        int NewLabel()
        {
            labelList.Add(-1);
            return labelList.Count - 1;
        }

        ////////// Register related //////////
        int RegAlloc()
        {
            regCount++;
            if (regCount > regMaxCount)
            {
                regMaxCount = regCount;
            }
            return regCount-1;
        }
        int RegAllocTemp()
        {
            regTempCount++;
            return RegAlloc();
        }
        void RegFreeTemp(int num)
        {
            RegFree(num);
            regTempCount -= num;
        }
        bool RegIsTemp(int reg)
        {
            return (reg >= regCount - regTempCount);
        }
        void RegFree(int num)
        {
            regCount -= num;
        }
        void RegClear()
        {
            regCount = 0;
            regMaxCount = 0;
        }

        ////////// Inst related //////////
        void InstGenR3(InstOp op, byte a, byte b, byte c)
        {
            instList.Add(new Inst { op = op, a = a, b = b, c = c });
        }
        void InstGenRI(InstOp op, byte a, ushort imm16)
        {
            Inst inst = new Inst();
            inst.op = op;
            inst.a = a;
            inst.b = (byte)(imm16 / 256);
            inst.c = (byte)(imm16 % 256);
            instList.Add(inst);
        }
        void InstGenI2(InstOp op, byte imm8, ushort imm16)
        {
            Inst inst = new Inst();
            inst.op = op;
            inst.a = imm8;
            inst.b = (byte)(imm16 / 256);
            inst.c = (byte)(imm16 % 256);
            instList.Add(inst);
        }
        void InstGenMov(byte a, byte b)
        {
            if (a != b) InstGenR3(InstOp.ADD, a, b, 255);
        }
        void InstLabelToOffset(int start_idx, int end_idx)
        {
            // Convert Label to offset
            for (int i = start_idx; i < end_idx; i++)
            {
                // Jump Operation(4)
                if (instList[i].op >= InstOp.JNZ && instList[i].op <= InstOp.JDEC)
                {
                    int dst = labelList[(int)instList[i].b * 256 + (int)instList[i].c];
                    if (dst < 0)
                    {
                        SetError(EnvError.UNDEFINED_LABEL);
                        break;
                    }
                    int delta = dst - i;
                    Inst inst = instList[i];
                    if (delta >= -32768 && delta < 32768)
                    {
                        int offset = delta + 32768;
                        inst.b = (byte)(offset / 256);
                        inst.c = (byte)(offset % 256);
                        instList[i] = inst;
                    }
                    else
                    {
                        SetError(EnvError.JUMP_OUT_OF_RANGE);
                        break;
                    }
                }
            }
        }

        ////////// Parser related //////////
        void ParserChangeState(ParserState new_state, bool stop_parser)
        {
            parserState = new_state;
            parserStop = stop_parser;
        }
        void ParserCallState(ParserState new_state, ParserState ret_state, bool stop_parser)
        {
            parserStack.Add(new ParserCallInfo {
                state = ret_state, 
                data1 = parserData1, 
                data2 = parserData2,
                data3 = parserData3,
            });
            ParserChangeState(new_state, stop_parser);
        }
        void ParserStateReturn(bool stop_parser)
        {
            ParserCallInfo info = parserStack[parserStack.Count - 1];
            parserData1 = info.data1;
            parserData2 = info.data2;
            parserData3 = info.data3;
            ParserChangeState(info.state, stop_parser);
            parserStack.RemoveAt(parserStack.Count - 1);    // stack pop
        }
        int ParserFindLoop()    // return -1 if not find
        {
            int stateIdx = -1;
            for (int i = parserStack.Count - 1; i >= 0; i--)
            {
                if (parserStack[i].state == ParserState.WHILE2 || parserStack[i].state == ParserState.FOR4)
                {
                    stateIdx = i;
                    break;
                }
            }
            return stateIdx;
        }
        void ParserEnterExpr(int p, ParserState ret_state, bool stop_parser)
        {
            ParserCallState(ParserState.EXPR1, ret_state, stop_parser);
            parserData1 = p;    // data1 : pcode
            ParserCallState(ParserState.ATOM1, ParserState.EXPR1, stop_parser);
        }
        void ParserEnterBlock(bool create_dict, int reg_offset, ParserState ret_state)
        {
            if (create_dict) symbolDicts.Add(new Dictionary<string, SymbolInfo>());
            ParserCallState(ParserState.BLOCK1, ret_state, true);
            parserData1 = regCount + reg_offset;                // block register count
        }
        void ParserEnterCall(in string func_name, ParserState ret_state)
        {
            ParserCallState(ParserState.CALL1, ret_state, true);
            SymbolInfo info = FindSymbol(func_name, 0, 0);
            if (info.symType == SymbolType.UNDEFINED)
                SetError(EnvError.UNDEFINED_SYMBOL);
            else if (info.symType == SymbolType.FUNCTION)
            {
                parserData1 = 0;            // Function argSize
                parserData2 = info.index;   // Function index
            }
            else SetError(EnvError.INVALID_TYPE);
        }
        void ParserCallReturn()
        {
            RegFreeTemp(parserData1);   // free argSize
            InstGenI2(InstOp.CALL, (byte)regCount, (ushort)parserData2);    // call argSize, func_idx
            parserDataRet = regCount;   // retrun value
            ParserStateReturn(true);
        }
        void ParserExpr1Bop(int p_code)
        {
            if (parserData1 < (p_code - p_code % 10))
            {
                parserData1 = p_code;
                parserData2 = parserDataRet;    // data2 : %left
                ParserEnterExpr(p_code, ParserState.EXPR2, true);
            }
            else ParserExpr1Retrun(p_code);
        }
        void ParserExpr1Retrun(int p_code)
        {
            if (RegIsTemp(parserDataRet)) RegFreeTemp(1);
            parserPcodeRet = p_code;
            ParserStateReturn(false);
        }
        void ParserExpr2Bop(InstOp op, int b, int c)
        {
            if (RegIsTemp(parserData2)) RegFreeTemp(1);
            int regIdx = RegAllocTemp();
            InstGenR3(op, (byte)regIdx, (byte)b, (byte)c);
            parserData2 = regIdx;
        }

        void ParserScan()
        {
            if (isPanic) return;
            parserStop = false;
            while (!parserStop)
            {
                switch (parserState)
                {
                    case ParserState.PROGRAM1:
                        if (tokenType == TokenType.IDENT)
                        {
                            if (tokenString == "using")
                                ParserCallState(ParserState.USING1, ParserState.PROGRAM1, true);
                            else if (tokenString == "function")
                                ParserCallState(ParserState.FUNCTION1, ParserState.PROGRAM1, true);
                            else SetError(EnvError.UNKNOWN_TOKEN);
                        }
                        else if (tokenType == TokenType.EOF)
                        {
                            ParserChangeState(ParserState.PROGRAM1, true);
                        }
                        break;

                    case ParserState.USING1:
                        if (tokenType == TokenType.IDENT)
                        {
                            parserCache = tokenString;
                            ParserChangeState(ParserState.USING2, true);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.USING2:
                        if (tokenType == TokenType.OPERATOR && tokenString == "=")
                        {
                            parserCache = tokenString;
                            ParserChangeState(ParserState.USING3, true);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.USING3:
                        if (tokenType == TokenType.IDENT)
                        {
                            SymbolInfo symInfo = FindSymbol(tokenString, symbolDicts.Count - 1, 0);
                            if (symInfo.symType == SymbolType.UNDEFINED)
                                SetError(EnvError.UNDEFINED_SYMBOL);
                            else
                                symbolDicts[symbolDicts.Count - 1][parserCache] = symInfo;
                        }
                        else if (tokenType == TokenType.INTEGER)
                        {
                            SymbolInfo symInfo = CreateInteger(tokenString);
                            symbolDicts[symbolDicts.Count - 1][parserCache] = symInfo;
                        }
                        else if (tokenType == TokenType.FLOAT)
                        {
                            SymbolInfo symInfo = CreateFloat(tokenString);
                            symbolDicts[symbolDicts.Count - 1][parserCache] = symInfo;
                        }
                        else if (tokenType == TokenType.STRING)
                        {
                            SymbolInfo symInfo = CreateString(tokenString);
                            symbolDicts[symbolDicts.Count - 1][parserCache] = symInfo;
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        ParserStateReturn(true);
                        break;

                    case ParserState.FUNCTION1:
                        if (tokenType == TokenType.FUNCT)
                        {
                            parserData1 = CreateFunction(tokenString); // data1 : funcInfoList index
                            if (funcInfoList.Count > 65536) SetError(EnvError.FUNCTION_OVERFLOW);
                            symbolDicts.Add(new Dictionary<string, SymbolInfo>());
                            RegClear();
                            labelList.Clear();
                            labelList.Add(instList.Count);   // function start
                            ParserChangeState(ParserState.FUNCTION2, true);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.FUNCTION2:
                        if (tokenType == TokenType.IDENT)
                        {
                            CreateVar(tokenString);
                            ParserChangeState(ParserState.FUNCTION3, true);
                        }
                        else if (tokenType == TokenType.OPERATOR && tokenString == ")")
                        {
                            ParserChangeState(ParserState.FUNCTION4, true);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.FUNCTION3:
                        if (tokenType == TokenType.OPERATOR)
                        {
                            if (tokenString == ",") ParserChangeState(ParserState.FUNCTION2, true);
                            else if (tokenString == ")") ParserChangeState(ParserState.FUNCTION4, true);
                            else SetError(EnvError.UNKNOWN_TOKEN);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.FUNCTION4:
                        if (tokenType == TokenType.OPERATOR && tokenString == "{")
                        {
                            ParserEnterBlock(false, -regCount, ParserState.FUNCTION5);
                        }
                        else
                        {
                            // Clear function-level symbol dictionary
                            symbolDicts.RemoveAt(symbolDicts.Count - 1);
                            labelList.Clear();
                            ParserStateReturn(false);
                        }
                        break;

                    case ParserState.FUNCTION5:
                        instList.Add(new Inst { op = InstOp.RET, a = 0, b = 0, c = 0 });   // RET
                        funcInfoList[parserData1] = new FuncInfo { 
                            codeAddr = labelList[0], 
                            regSize = regMaxCount <= 0 ? 1 : regMaxCount
                        };
                        if (regMaxCount > 255) SetError(EnvError.REGISTER_OVERFLOW);
                        else if (constNumberList.Count > 65536) SetError(EnvError.CONSTANT_OVERFLOW);
                        else if (labelList.Count > 65536) SetError(EnvError.LABEL_OVERFLOW);
                        else InstLabelToOffset(labelList[0], instList.Count);
                        ParserStateReturn(false);
                        labelList.Clear();
                        break;

                    case ParserState.BLOCK1:
                        if (tokenType == TokenType.IDENT)
                        {
                            if (tokenString == "break")
                            {
                                int stateIdx = ParserFindLoop();
                                if (stateIdx >= 0)
                                {
                                    ParserCallInfo info = parserStack[stateIdx];
                                    // data1 : LOOP_START data2 : LOOP_END
                                    if (info.state == ParserState.FOR4)
                                    {
                                        // data2 < 0 : until
                                        if (info.data2 < 0) InstGenRI(InstOp.JZR, 255, (ushort)(-info.data2));
                                        // data2 > 0 : to/downto
                                        else InstGenRI(InstOp.JZR, 255, (ushort)info.data2);
                                    }
                                    else InstGenRI(InstOp.JZR, 255, (ushort)info.data2);
                                }
                                else SetError(EnvError.BREAK_OUTSIDE_LOOP);
                                ParserChangeState(ParserState.BLOCK1, true);
                            }
                            else if (tokenString == "continue")
                            {
                                int stateIdx = ParserFindLoop();
                                if (stateIdx >= 0)
                                {
                                    ParserCallInfo info = parserStack[stateIdx];
                                    // data1 : LOOP_START data2 : LOOP_END
                                    if (info.state == ParserState.FOR4)
                                    {
                                        // data1 < 0 : downto
                                        if (info.data1 < 0) InstGenRI(InstOp.JDEC, (byte)info.data3, (ushort)(-info.data1));
                                        // data1 > 0 : to|until
                                        else InstGenRI(InstOp.JINC, (byte)info.data3, (ushort)info.data1);
                                    }
                                    else InstGenRI(InstOp.JZR, 255, (ushort)info.data1);
                                }
                                else SetError(EnvError.CONTINUE_OUTSIDE_LOOP);
                                ParserChangeState(ParserState.BLOCK1, true);
                            }
                            else if (tokenString == "using")
                            {
                                ParserCallState(ParserState.USING1, ParserState.BLOCK1, true);
                            }
                            else if (tokenString == "if")
                            {
                                ParserCallState(ParserState.IF1, ParserState.BLOCK1, true);
                                parserData1 = NewLabel();  // data1 : ELSE
                                parserData2 = NewLabel();  // data2 : ENDIF
                                ParserEnterExpr(0, ParserState.IF1, true);
                            }
                            else if (tokenString == "while")
                            {
                                ParserCallState(ParserState.WHILE1, ParserState.BLOCK1, true);
                                parserData1 = NewLabel();  // data1 : LOOP_START
                                parserData2 = NewLabel();  // data2 : LOOP_END
                                labelList[parserData1] = instList.Count;    // label :LOOP_START
                                ParserEnterExpr(0, ParserState.WHILE1, true);
                            }
                            else if (tokenString == "for")
                            {
                                ParserCallState(ParserState.FOR1, ParserState.BLOCK1, true);
                            }
                            else if (tokenString == "return")
                            {
                                ParserCallState(ParserState.RETURN1, ParserState.BLOCK1, true);
                            }
                            else if (tokenString == "goto")
                            {
                                ParserCallState(ParserState.GOTO1, ParserState.BLOCK1, true);
                            }
                            else if (tokenString == "var")
                            {
                                ParserCallState(ParserState.VAR1, ParserState.BLOCK1, true);
                            }
                            else
                            {
                                ParserCallState(ParserState.ASSIGN1, ParserState.BLOCK1, true);
                                SymbolInfo info = FindSymbol(tokenString, symbolDicts.Count - 1, 0);
                                if (info.symType == SymbolType.UNDEFINED)
                                    SetError(EnvError.UNDEFINED_SYMBOL);
                                else if (info.symType == SymbolType.VAR)
                                    parserData1 = info.index; // data1 : dst reg
                                else SetError(EnvError.INVALID_TYPE);
                            }
                        }
                        else if (tokenType == TokenType.LABEL)
                        {
                            int idx = CreateLabel(tokenString + ":");
                            labelList[idx] = instList.Count;
                            ParserChangeState(ParserState.BLOCK1, true);
                        }
                        else if (tokenType == TokenType.FUNCT)
                        {
                            ParserEnterCall(tokenString, ParserState.BLOCK1);
                        }
                        else if (tokenType == TokenType.OPERATOR)
                        {
                            if (tokenString == "{")
                            {
                                ParserEnterBlock(true, 0, ParserState.BLOCK1);
                            }
                            else if (tokenString == "}")
                            {
                                RegFree(regCount - parserData1);
                                symbolDicts.RemoveAt(symbolDicts.Count - 1);
                                ParserStateReturn(true);
                            }
                            else SetError(EnvError.UNKNOWN_TOKEN);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.ASSIGN1:
                        if (tokenType == TokenType.OPERATOR && tokenString == "=")
                            ParserEnterExpr(0, ParserState.ASSIGN2, true);
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.ASSIGN2:
                        InstGenMov((byte)parserData1, (byte)parserDataRet);
                        ParserStateReturn(false);
                        break;

                    case ParserState.VAR1:
                        if (tokenType == TokenType.IDENT)
                        {
                            ParserChangeState(ParserState.ASSIGN1, true);
                            parserData1 = CreateVar(tokenString); // data1 : dst reg
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.IF1:
                        InstGenRI(InstOp.JZR, (byte)parserDataRet, (byte)parserData1); // jzr %condition, :ELSE
                        if (tokenType == TokenType.OPERATOR && tokenString == "{")
                        {
                            ParserEnterBlock(true, 0, ParserState.IF2);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.IF2:
                        InstGenRI(InstOp.JZR, 255, (byte)parserData2); // jmp :ENDIF
                        labelList[parserData1] = instList.Count; // label :ELSE
                        if (tokenType == TokenType.IDENT && tokenString == "else")
                            ParserChangeState(ParserState.IF3, true);
                        else 
                            ParserChangeState(ParserState.IF4, false);
                        break;

                    case ParserState.IF3:
                        if (tokenType == TokenType.IDENT && tokenString == "if")
                        {
                            ParserCallState(ParserState.IF1, ParserState.IF4, true);
                            parserData1 = NewLabel();  // data1 : ELSE data2 : ENDIF
                            ParserEnterExpr(0, ParserState.IF1, true);
                        }
                        else if (tokenType == TokenType.OPERATOR && tokenString == "{")
                        {
                            ParserEnterBlock(true, 0, ParserState.IF4);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.IF4:
                        labelList[parserData2] = instList.Count; // label :ENDIF
                        ParserStateReturn(false);
                        break;

                    case ParserState.WHILE1:
                        InstGenRI(InstOp.JZR, (byte)parserDataRet, (ushort)parserData2); // jzr %condition, :LOOP_END
                        if (tokenType == TokenType.OPERATOR && tokenString == "{")
                        {
                            ParserEnterBlock(true, 0, ParserState.WHILE2);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.WHILE2:
                        InstGenRI(InstOp.JZR, 255, (ushort)parserData1); // jmp :LOOP_START
                        labelList[parserData2] = instList.Count; // label :LOOP_END
                        ParserStateReturn(false);
                        break;

                    case ParserState.FOR1:
                        symbolDicts.Add(new Dictionary<string, SymbolInfo>());
                        if (tokenType == TokenType.IDENT)
                        {
                            int regIdx = CreateVar(tokenString);
                            RegAlloc();                        // reg2  : %compare
                            parserData1 = NewLabel();          // data1 : LOOP_START
                            parserData2 = NewLabel();          // data2 : LOOP_END
                            parserData3 = regIdx;              // data3 : %count
                            ParserCallState(ParserState.ASSIGN1, ParserState.FOR2, true);
                            parserData1 = regIdx;              // data1 : %count
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.FOR2:
                        if (tokenType == TokenType.IDENT)
                        {
                            labelList[parserData1] = instList.Count;
                            if (tokenString == "to")
                            { }                             // data1 > 0 data2 > 0
                            else if (tokenString == "downto")
                            { parserData1 = -parserData1; } // data1 < 0 data2 > 0
                            else if (tokenString == "until")
                            { parserData2 = -parserData2; } // data1 > 0 data2 < 0
                            else SetError(EnvError.UNKNOWN_TOKEN);
                            ParserEnterExpr(0, ParserState.FOR3, true);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.FOR3:
                        InstGenMov((byte)(parserData3 + 1), (byte)parserDataRet); // mov %compare %compare_val
                        if (parserData1 < 0)    // downto
                        {
                            labelList[-parserData1] = instList.Count; // label :LOOP_START
                            byte regTemp = (byte)(RegAlloc());
                            InstGenR3(InstOp.SGE, regTemp, (byte)parserData3, (byte)(parserData3 + 1));    // sge %temp %count %compare
                            InstGenRI(InstOp.JZR, regTemp, (ushort)parserData2);    // jzr %temp :LOOP_END
                            RegFree(1);
                        }
                        else if (parserData2 < 0)   // until
                        {
                            labelList[parserData1] = instList.Count; // label :LOOP_START
                            byte regTemp = (byte)(RegAlloc());
                            InstGenR3(InstOp.SLT, regTemp, (byte)parserData3, (byte)(parserData3 + 1));    // slt %temp %count %compare
                            InstGenRI(InstOp.JZR, regTemp, (ushort)(-parserData2));    // jzr %temp :LOOP_END
                            RegFree(1);
                        }
                        else // to
                        {
                            labelList[parserData1] = instList.Count; // label :LOOP_START
                            byte regTemp = (byte)(RegAlloc());
                            InstGenR3(InstOp.SGE, regTemp, (byte)(parserData3 + 1), (byte)parserData3);    // sle %temp %count %compare
                            InstGenRI(InstOp.JZR, regTemp, (ushort)parserData2);    // jzr %temp :LOOP_END
                            RegFree(1);
                        }
                        if (tokenType == TokenType.OPERATOR && tokenString == "{")
                        {
                            ParserEnterBlock(false, -2, ParserState.FOR4);
                        }
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.FOR4:
                        // data1 < 0 : downto
                        if (parserData1 < 0) InstGenRI(InstOp.JDEC, (byte)parserData3, (ushort)(-parserData1));
                        // data1 > 0 : to|until
                        else InstGenRI(InstOp.JINC, (byte)parserData3, (ushort)parserData1);
                        labelList[parserData2 < 0 ? -parserData2 : parserData2] = instList.Count; // label :LOOP_END
                        ParserStateReturn(false);
                        break;

                    case ParserState.RETURN1:
                        if (tokenType == TokenType.OPERATOR && tokenString == "}")
                        {
                            InstGenI2(InstOp.RET, 0, 0);    // ret
                            ParserStateReturn(false);
                        }
                        else ParserEnterExpr(0, ParserState.RETURN2, false);
                        break;

                    case ParserState.RETURN2:
                        InstGenMov(0, (byte)parserDataRet); // mov %ret %ret_val
                        InstGenI2(InstOp.RET, 0, 0);    // ret
                        ParserStateReturn(false);
                        break;

                    case ParserState.GOTO1:
                        { 
                            int idx = CreateLabel(tokenString + ":");
                            InstGenRI(InstOp.JZR, 255, (ushort)idx);
                            ParserStateReturn(true);
                        }
                        break;

                    case ParserState.CALL1:
                        if (tokenType == TokenType.OPERATOR && tokenString == ")")
                            ParserCallReturn();
                        else ParserEnterExpr(0, ParserState.CALL2, false);
                        break;

                    case ParserState.CALL2:
                        {
                            int idx = RegAllocTemp();
                            InstGenMov((byte)idx, (byte)parserDataRet); // mov %argx %argx_val
                            parserData1++;
                        }
                        if (tokenType == TokenType.OPERATOR && tokenString == ")")
                            ParserCallReturn();
                        else if (tokenType == TokenType.OPERATOR && tokenString == ",")
                            ParserChangeState(ParserState.CALL1, true);
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.ATOM1:
                        if (tokenType == TokenType.OPERATOR)
                        {
                            if (tokenString == "(") ParserEnterExpr(0, ParserState.ATOM2, true);
                            else if (tokenString == "-" || tokenString == "!")
                            {
                                parserData1 = (tokenString == "-") ? 60 : 61;   // data1 : pcode
                                ParserCallState(ParserState.ATOM1, ParserState.ATOM3, true);
                            }
                            else SetError(EnvError.UNKNOWN_TOKEN);
                        }
                        else if (tokenType == TokenType.FUNCT)
                            ParserEnterCall(tokenString, ParserState.ATOM4);
                        else
                        {
                            SymbolInfo symInfo = new SymbolInfo { symType = SymbolType.IMMEDIATE, index = 0 };
                            if (tokenType == TokenType.INTEGER) symInfo = CreateInteger(tokenString);
                            else if (tokenType == TokenType.FLOAT) symInfo = CreateFloat(tokenString);
                            else if (tokenType == TokenType.STRING) symInfo = CreateString(tokenString);
                            else if (tokenType == TokenType.IDENT) symInfo = FindSymbol(tokenString, symbolDicts.Count - 1, 0);
                            else SetError(EnvError.UNKNOWN_TOKEN);
                            if (symInfo.symType == SymbolType.UNDEFINED) SetError(EnvError.UNDEFINED_SYMBOL);
                            else if (symInfo.symType == SymbolType.VAR) parserDataRet = symInfo.index;
                            else if (symInfo.symType == SymbolType.IMMEDIATE)
                            {
                                int idx = RegAllocTemp();
                                InstGenRI(InstOp.LDI, (byte)idx, (ushort)symInfo.index);    // ldi %temp #imm
                                parserDataRet = idx;
                            }
                            else if (symInfo.symType == SymbolType.CONSTANT)
                            {
                                int idx = RegAllocTemp();
                                InstGenRI(InstOp.LDC, (byte)idx, (ushort)symInfo.index);    // ldc %temp const_idx
                                parserDataRet = idx;
                            }
                            else SetError(EnvError.INVALID_TYPE);
                            ParserStateReturn(true);
                        }
                        break;

                    case ParserState.ATOM2:
                        if (tokenType == TokenType.OPERATOR && tokenString == ")")
                            ParserStateReturn(true);
                        else SetError(EnvError.UNKNOWN_TOKEN);
                        break;

                    case ParserState.ATOM3:
                        if (parserData1 == 60)  // -     sub %dst zero %src
                        {
                            if (RegIsTemp(parserDataRet))
                                InstGenR3(InstOp.SUB, (byte)parserDataRet, 255, (byte)parserDataRet);
                            else
                            {
                                int idx = RegAllocTemp();
                                InstGenR3(InstOp.SUB, (byte)idx, 255, (byte)parserDataRet);
                                parserDataRet = idx;
                            }
                        }
                        else // !    seq %dst zero %src
                        {
                            if (RegIsTemp(parserDataRet))
                                InstGenR3(InstOp.SEQ, (byte)parserDataRet, 255, (byte)parserDataRet);
                            else
                            {
                                int idx = RegAllocTemp();
                                InstGenR3(InstOp.SEQ, (byte)idx, 255, (byte)parserDataRet);
                                parserDataRet = idx;
                            }
                        }
                        ParserStateReturn(false);
                        break;

                    case ParserState.ATOM4:
                        {
                            int idx = RegAllocTemp();   // %ret
                            parserDataRet = idx;
                        }
                        ParserStateReturn(false);
                        break;

                    case ParserState.EXPR1:
                        if (tokenType == TokenType.OPERATOR)
                        {
                            if (tokenString == "*") ParserExpr1Bop(50);          // pcode '*'  == 50
                            else if (tokenString == "/") ParserExpr1Bop(51);     // pcode '/'  == 51
                            else if (tokenString == "+") ParserExpr1Bop(40);     // pcode '+'  == 40
                            else if (tokenString == "-") ParserExpr1Bop(41);     // pcode '-'  == 41
                            else if (tokenString == "==") ParserExpr1Bop(30);    // pcode '==' == 30
                            else if (tokenString == "!=") ParserExpr1Bop(31);    // pcode '!=' == 31
                            else if (tokenString == ">") ParserExpr1Bop(32);     // pcode '>'  == 32
                            else if (tokenString == "<") ParserExpr1Bop(33);     // pcode '<'  == 33
                            else if (tokenString == ">=") ParserExpr1Bop(34);    // pcode '>=' == 34
                            else if (tokenString == "<=") ParserExpr1Bop(35);    // pcode '<=' == 35
                            else ParserExpr1Retrun(0);
                        }
                        else if (tokenType == TokenType.IDENT)
                        {
                            if (tokenString == "and")
                            {
                                if (parserData1 < 20)
                                {
                                    parserData2 = parserDataRet;
                                    if (!RegIsTemp(parserDataRet))
                                    {
                                        parserData2 = RegAllocTemp();
                                        InstGenMov((byte)parserData2, (byte)parserDataRet); // mov %result %left
                                    }
                                    parserData3 = NewLabel();   // data3 : :skip
                                    InstGenRI(InstOp.JZR, (byte)parserData2, (ushort)parserData3);  // jzr %result :skip
                                    parserData1 = 20;
                                    ParserEnterExpr(20, ParserState.EXPR2, true);
                                }
                                else ParserExpr1Retrun(20);
                            }
                            else if (tokenString == "or")
                            {
                                if (parserData1 < 10)
                                {
                                    parserData2 = parserDataRet;
                                    if (!RegIsTemp(parserDataRet))
                                    {
                                        parserData2 = RegAllocTemp();
                                        InstGenMov((byte)parserData2, (byte)parserDataRet); // mov %result %left
                                    }
                                    parserData3 = NewLabel();   // data3 : :skip
                                    InstGenRI(InstOp.JNZ, (byte)parserData2, (ushort)parserData3);  // jnz %result :skip
                                    parserData1 = 10;
                                    ParserEnterExpr(10, ParserState.EXPR2, true);
                                }
                                else ParserExpr1Retrun(10);
                            }
                            else ParserExpr1Retrun(0);
                        }
                        else ParserExpr1Retrun(0);
                        break;

                    case ParserState.EXPR2:
                        if (parserData1 == 50) ParserExpr2Bop(InstOp.MUL, parserData2, parserDataRet); // mul
                        else if (parserData1 == 51) ParserExpr2Bop(InstOp.DIV, parserData2, parserDataRet); // div
                        else if (parserData1 == 40) ParserExpr2Bop(InstOp.ADD, parserData2, parserDataRet); // add
                        else if (parserData1 == 41) ParserExpr2Bop(InstOp.SUB, parserData2, parserDataRet); // sub
                        else if (parserData1 == 30) ParserExpr2Bop(InstOp.SEQ, parserData2, parserDataRet); // ==
                        else if (parserData1 == 31) ParserExpr2Bop(InstOp.SNE, parserData2, parserDataRet); // !=
                        else if (parserData1 == 32) ParserExpr2Bop(InstOp.SLT, parserDataRet, parserData2); // >
                        else if (parserData1 == 33) ParserExpr2Bop(InstOp.SLT, parserData2, parserDataRet); // <
                        else if (parserData1 == 34) ParserExpr2Bop(InstOp.SGE, parserData2, parserDataRet); // >=
                        else if (parserData1 == 35) ParserExpr2Bop(InstOp.SGE, parserDataRet, parserData2); // <=
                        else if (parserData1 == 20 || parserData1 == 10)
                        {
                            InstGenMov((byte)parserData2, (byte)parserDataRet);
                            labelList[parserData3] = instList.Count;
                        }
                        else SetError(EnvError.UNKNOWN_PCODE);
                        if (parserPcodeRet > 0)
                        {
                            if (parserPcodeRet == 20)
                            {
                                int regLeft = parserData2;
                                if (!RegIsTemp(regLeft))
                                {
                                    parserData2 = RegAllocTemp();
                                    InstGenMov((byte)parserData2, (byte)regLeft); // mov %result %left
                                }
                                parserData3 = NewLabel();   // data3 : :skip
                                InstGenRI(InstOp.JZR, (byte)parserData2, (ushort)parserData3);  // jzr %result :skip
                            }
                            else if (parserPcodeRet == 10)
                            {
                                int regLeft = parserData2;
                                if (!RegIsTemp(regLeft))
                                {
                                    parserData2 = RegAllocTemp();
                                    InstGenMov((byte)parserData2, (byte)regLeft); // mov %result %left
                                }
                                parserData3 = NewLabel();   // data3 : :skip
                                InstGenRI(InstOp.JNZ, (byte)parserData2, (ushort)parserData3);  // jnz %result :skip
                            }
                            parserData1 = parserPcodeRet;
                            ParserEnterExpr(parserData1, ParserState.EXPR2, true);
                        }
                        else
                        {
                            if (RegIsTemp(parserData2)) RegFreeTemp(1);
                            parserDataRet = parserData2;
                            ParserStateReturn(false);
                        }
                        break;

                    default: SetError(EnvError.UNKNOWN_PARSER_STATE); break;
                }
            }
        }

    }

    public class CoidState
    {
        struct CallInfo
        {
            public int savedFp;    //frame pointer
            public int savedPc;    //program counter
        }

        ////////// Constant ////////// 
        public const int STATE_WAITING = 0x1;
        public const int STATE_FINISH = 0x2;
        public const int STATE_PANIC = 0x4;

        ////////// Environment //////////
        CoidEnv env;

        ////////// Variable //////////
        public int stateCode = STATE_FINISH;    // [0] waiting [1] finish [2] panic
        public StateError stateError = StateError.NO_ERROR;
        public int callCode = 0;    // >= 0
        // Stack
        List<double> dataStack;
        List<CallInfo> callStack;
        // State Register
        int pc = 0;    //program counter
        int fp = 0;    //main frame pointer
        int csp = 0;   //call stack pointer
        int callSaveReg = 0;
        // Limits
        int maxDataStackSize = 0;
        int maxCallStackSize = 0;

        public CoidState(CoidEnv coid_env, int max_datastack_size, int max_callstack_size)
        {
            env = coid_env;
            dataStack = new List<double>(max_datastack_size);
            callStack = new List<CallInfo>(max_callstack_size);
        }

        public void SetEntry(in string user_func_name, params double[] args)
        {
            Clear();
            SymbolInfo info = env.FindGlobalSymbol(user_func_name);
            if (info.symType != SymbolType.FUNCTION)
            {
                SetError(StateError.UNDEFINED_FUNCTION);
                return;
            }
            CallFunc(info.index, 0);
            stateCode &= ~STATE_FINISH;
            for (int i = 0; i < args.Length; i++) dataStack[i] = args[i];
        }

        public void SetRet(double val)
        {
            if ((stateCode & STATE_WAITING) == 0) return;
            dataStack[fp] = val;
            fp -= callSaveReg;
            stateCode &= ~STATE_WAITING;
        }

        public double GetArg(int index)
        {
            return dataStack[fp + index];
        }

        public string GetArgString(int index)
        {
            return env.constStringList[(int)GetArg(index)];
        }

        public double GetRet() { return GetArg(0); }
        public string GetRetString() { return GetArgString(0); }

        public void Clear()
        {
            pc = 0;
            fp = 0;
            csp = 0;
            stateCode = STATE_FINISH;
            stateError = StateError.NO_ERROR;
            callCode = 0;
            callSaveReg = 0;
        }

        public void Execute()
        {
            if (stateCode > 0) return;
            Inst inst = env.instList[pc];
            if (inst.op < InstOp.JNZ)
            {
                double b = inst.b == 255 ? 0 : dataStack[fp + inst.b];
                double c = inst.c == 255 ? 0 : dataStack[fp + inst.c];
                switch (inst.op)
                {
                    case InstOp.ADD:    // ADD | 0 | A | B | C | type-R3
                        dataStack[fp + inst.a] = b + c; break;
                    case InstOp.SUB:    // SUB | 1 | A | B | C | type-R3 
                        dataStack[fp + inst.a] = b - c; break;
                    case InstOp.MUL:    // MUL | 2 | A | B | C | type-R3
                        dataStack[fp + inst.a] = b * c; break;
                    case InstOp.DIV:    // DIV | 3 | A | B | C | type-R3
                        dataStack[fp + inst.a] = b / c; break;
                    case InstOp.SLT:    // SLT | 4 | A | B | C | type-R3
                        dataStack[fp + inst.a] = (b < c) ? 1 : 0; break;
                    case InstOp.SGE:    // SGE | 5 | A | B | C | type-R3
                        dataStack[fp + inst.a] = (b >= c) ? 1 : 0; break;
                    case InstOp.SEQ:    // SEQ | 6 | A | B | C | type-R3
                        dataStack[fp + inst.a] = (b == c) ? 1 : 0; break;
                    case InstOp.SNE:    // SNE | 7 | A | B | C | type-R3
                        dataStack[fp + inst.a] = (b != c) ? 1 : 0; break;
                    default:
                        SetError(StateError.UNKNOWN_INSTRUCTION); break;
                }
            }
            else if (inst.op < InstOp.CALL)
            {
                double a = inst.a == 255 ? 0 : dataStack[fp + inst.a];
                int imm = (int)inst.b * 256 + (int)inst.c;
                switch (inst.op)
                {
                    case InstOp.JNZ:    // JNZ | 8 | A |uint16 | type-RI
                        if (a != 0) pc += imm - 32768; break;
                    case InstOp.JZR:    // JZR | 9 | A |uint16 | type-RI
                        if (a == 0) pc += imm - 32768; break;
                    case InstOp.JINC:   // JINC|10 | A |uint16 | type-RI
                        dataStack[fp + inst.a] += 1; 
                        pc += imm - 32768; 
                        break;
                    case InstOp.JDEC:   // JDEC|11 | A |uint16 | type-RI
                        dataStack[fp + inst.a] -= 1;
                        pc += imm - 32768;
                        break;
                    default:
                        SetError(StateError.UNKNOWN_INSTRUCTION); break;
                }
            }
            else
            {
                int imm = (int)inst.b * 256 + (int)inst.c;
                switch (inst.op)
                {
                    case InstOp.CALL:   // CALL|12 |u8 |uint16 | type-I2
                        CallFunc(imm, inst.a); break;
                    case InstOp.RET:    // RET |13 | 0 |   0   | type-I2
                        csp -= 1;
                        fp = callStack[csp].savedFp;
                        pc = callStack[csp].savedPc;
                        if (csp == 0) stateCode &= STATE_FINISH;
                        break;
                    case InstOp.LDC:   // LDC |14 | A |uint16 | type-RI
                        dataStack[fp + inst.a] = env.constNumberList[imm]; break;
                    case InstOp.LDI:   // LDI |15 | A |uint16 | type-RI
                        dataStack[fp + inst.a] = imm; break;
                    default:
                        SetError(StateError.UNKNOWN_INSTRUCTION); break;
                }
            }
        }

        ////////// Error Handling //////////
        void SetError(StateError err_code)
        {
            stateError = err_code;
            stateCode &= STATE_PANIC;
        }

        ////////// Runtime //////////
        void CallFunc(int func_idx, int save_reg)
        {
            FuncInfo info = env.funcInfoList[func_idx];
            if (info.regSize < 0)
            {
                SetError(StateError.UNDEFINED_FUNCTION);
                return;
            }
            if (fp + save_reg + info.regSize > maxCallStackSize)
            {
                SetError(StateError.DATASTACK_OVERFLOW);
                return;
            }
            if (info.codeAddr >= 0)
            {
                if (csp + 1 > maxCallStackSize)
                {
                    SetError(StateError.CALLSTACK_OVERFLOW); return;
                }
                callStack[csp] = new CallInfo { savedFp = fp, savedPc = pc + 1 };
                csp += 1;
                fp += save_reg;
                pc = info.codeAddr;
            }
            else
            {
                callCode = -info.codeAddr - 1;    //call system function
                callSaveReg = save_reg;
                fp += save_reg;
                stateCode &= STATE_WAITING;
            }
        }
    }
}