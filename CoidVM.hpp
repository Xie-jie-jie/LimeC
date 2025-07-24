#ifndef _COIDVM_HPP_
#define _COIDVM_HPP_

#include <vector>
#include <unordered_map>
#include <string>
#include <initializer_list>

namespace Coid {
	using byte = unsigned char;
	using ushort = unsigned short;
	using string = std::string;
    enum EnvError {
		NO_ENV_ERROR,
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
	};
    enum StateError {
		NO_STATE_ERROR,
		UNDEFINED_FUNCTION,
		CALLSTACK_OVERFLOW,
		DATASTACK_OVERFLOW,
		UNKNOWN_INSTRUCTION,
		INDEX_OUT_OF_BOUNDS
	};
    enum class InstOp : byte {
		OP_ADD,
		OP_SUB,
		OP_MUL,
		OP_DIV,
		OP_SLT,
		OP_SGE,
		OP_SEQ,
		OP_SNE,
		OP_JNZ,
		OP_JZR,
		OP_JINC,
		OP_JDEC,
		OP_CALL,
		OP_RET,
		OP_LDC,
		OP_LDI
	};
    struct Inst {
		InstOp op; 
		byte a, b, c;
	};
    struct FuncInfo {
		int codeAddr;
		int regSize;
	};
    enum SymbolType {
		TYPE_UNDEFINED,
		TYPE_VAR,
		TYPE_FUNCTION,
		TYPE_CONSTANT,
		TYPE_IMMEDIATE,
		TYPE_ALABEL
	};
	struct SymbolInfo {
		SymbolType symType;
		int index;
	};
    enum TokenType {
        TYPE_WS,
        TYPE_EOF,
        TYPE_IDENT,
        TYPE_FUNCT,
        TYPE_LABEL,
        TYPE_INTEGER,
        TYPE_FLOAT,
        TYPE_STRING,
        TYPE_OPERATOR
	};
	enum LexerState {
		STATE_WS,
		STATE_IDENT,
		STATE_INTEGER,
		STATE_FLOAT,
		STATE_STRING,
		STATE_ESCAPE,
		STATE_OPERATOR,
		STATE_COMMENT
	};
	enum ParserState {
		STATE_PROGRAM1,
		STATE_USING1, 
		STATE_USING2, 
		STATE_USING3,
		STATE_FUNCTION1, 
		STATE_FUNCTION2, 
		STATE_FUNCTION3, 
		STATE_FUNCTION4, 
		STATE_FUNCTION5,
		STATE_BLOCK1,
		STATE_ASSIGN1, 
		STATE_ASSIGN2,
		STATE_VAR1,
		STATE_IF1, 
		STATE_IF2, 
		STATE_IF3, 
		STATE_IF4,
		STATE_WHILE1, 
		STATE_WHILE2,
		STATE_FOR1, 
		STATE_FOR2, 
		STATE_FOR3, 
		STATE_FOR4,
		STATE_RETURN1, 
		STATE_RETURN2,
		STATE_GOTO1,
		STATE_CALL1, 
		STATE_CALL2,
		STATE_ATOM1, 
		STATE_ATOM2, 
		STATE_ATOM3, 
		STATE_ATOM4,
		STATE_EXPR1, 
		STATE_EXPR2
	};
	struct ParserCallInfo {
		ParserState state;
		int data1;
		int data2;
		int data3;
	};
	struct CallInfo {
		int savedFp;    //frame pointer
		int savedPc;    //program counter
	};
    class CoidEnv {
	public:
		//////////////////// Variables ////////////////////
		////////// Runtime Related //////////
		// Program
		std::vector<Inst> instList;
		std::vector<FuncInfo> funcInfoList;
		// Constant
		std::vector<double> constNumberList;
		std::vector<string> constStringList;
		std::unordered_map<double, int> constNumberCache;
		std::unordered_map<string, int> constStringCache;
		////////// Compile Related //////////
		bool isPanic = false;
		EnvError envError = EnvError::NO_ENV_ERROR;
		string inputName = "";
		int lineCounter = 0;
		////////// Lexer Related //////////
		string lexerCache = "";
		LexerState lexerState = LexerState::STATE_WS;
		char cc = ' ';  // char cache
		TokenType tokenType = TokenType::TYPE_WS;
		string tokenString = "";
		////////// Parser Related //////////
		std::vector<std::unordered_map<string, SymbolInfo>> symbolDicts;
		std::vector<int> labelList;
		ParserState parserState = ParserState::STATE_PROGRAM1;
		int parserData1 = 0;
		int parserData2 = 0;
		int parserData3 = 0;
		int parserDataRet = 0;
		int parserPcodeRet = 0;
		std::vector<ParserCallInfo> parserStack;
		bool parserStop = true;
		string parserCache = "";
		int regCount = 0;
		int regMaxCount = 0;
		int regTempCount = 0;
		//////////////////// Functions ////////////////////
		CoidEnv() {
			symbolDicts.push_back(std::unordered_map<string, SymbolInfo>());
			parserState = ParserState::STATE_PROGRAM1;
		}
		void NewInput(const string& input_name) {
			inputName = input_name;
			lineCounter = 0;
		}
		void InputChar(char c) {
			cc = c;
			LexerScan();
			if (tokenType != TokenType::TYPE_WS) ParserScan();
		}
		void CloseInput() {
			InputChar('\n');
			tokenType = TokenType::TYPE_EOF;
			ParserScan();
		}
		void ClearError() {
			isPanic = false;
			envError = EnvError::NO_ENV_ERROR;
			lexerCache = "";
			lexerState = LexerState::STATE_WS;
			tokenType = TokenType::TYPE_WS;
			tokenString = "";
			symbolDicts.resize(1);
			labelList.clear();
			parserState = ParserState::STATE_PROGRAM1;
			parserStack.clear();
			parserCache = "";
			regCount = 0;
			regMaxCount = 0;
			regTempCount = 0;
		}
		void Clear()
		{
			ClearError();
			instList.clear();
			funcInfoList.clear();
			constNumberList.clear();
			constNumberCache.clear();
			constStringList.clear();
			constStringCache.clear();
			symbolDicts[0].clear();
		}
		void SetSysFunction(const string& func_name, int call_code)   // call_code >= 0
		{
			funcInfoList.push_back(FuncInfo{-call_code-1, 1});
			symbolDicts[0][func_name] = SymbolInfo{SymbolType::TYPE_FUNCTION, (int)funcInfoList.size()-1};
		}
		SymbolInfo FindGlobalSymbol(const string&  symbol_name) {
			return FindSymbol(symbol_name, 0, 0);
		}
	private:
		////////// Error Handling //////////
		void SetError(EnvError env_error) {
			isPanic = true; 
			envError = env_error;
			parserStop = true;
		}
		////////// Lexer related //////////
		static bool IsWhiteSpace(char c) {
			if (c == ' ' || c == '\t' || c == '\v' || c == '\r' || c == '\n' || c == '\f')
				return true; 
			else return false;
		}
		static bool IsLetter(char c) {
			if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
				return true;
			else return false;
		}
		static bool IsNumber(char c) {
			if (c >= '0' && c <= '9')
				return true;
			else return false;
		}
		static bool IsOp(char c) {
			// Op: ( ) * + , - < = > ! / { }
			if ((c >= '(' && c <= '-') || (c >= '<' && c <= '>')
			   || c == '!' || c == '/' || c == '{' || c == '}')
				return true;
			else return false;
		}
		void LexerChangeState(LexerState new_state, bool truncate, bool append) {
			lexerState = new_state;
			if (truncate) lexerCache = "";
			if (append) lexerCache += cc;
			tokenType = TokenType::TYPE_WS;
		}
		void LexerOutput(TokenType tk_type, LexerState new_state, bool truncate, bool append) {
			tokenString = lexerCache;
			LexerChangeState(new_state, truncate, append);
			tokenType = tk_type;
		}
		void LexerScan() {
			if (isPanic) return;
			if (cc == '\n') lineCounter += 1;
			switch (lexerState) {
				case LexerState::STATE_WS:
					if (IsWhiteSpace(cc)) LexerChangeState(LexerState::STATE_WS, false, false);
					else if (IsLetter(cc)) LexerChangeState(LexerState::STATE_IDENT, true, true);
					else if (IsNumber(cc)) LexerChangeState(LexerState::STATE_INTEGER, true, true);
					else if (cc == '.') LexerChangeState(LexerState::STATE_FLOAT, true, true);
					else if (cc == '"') LexerChangeState(LexerState::STATE_STRING, true, false);
					else if (IsOp(cc)) LexerChangeState(LexerState::STATE_OPERATOR, true, true);
					else SetError(EnvError::UNKNOWN_TOKEN); break;
				case LexerState::STATE_IDENT:
					if (IsLetter(cc) || IsNumber(cc)) LexerChangeState(LexerState::STATE_IDENT, false, true);
					else if (cc == '(') LexerOutput(TokenType::TYPE_FUNCT, LexerState::STATE_WS, true, false);
					else if (cc == ':') LexerOutput(TokenType::TYPE_LABEL, LexerState::STATE_WS, true, false);
					else if (IsOp(cc)) LexerOutput(TokenType::TYPE_IDENT, LexerState::STATE_OPERATOR, true, true);
					else if (IsWhiteSpace(cc)) LexerOutput(TokenType::TYPE_IDENT, LexerState::STATE_WS, true, false);
					else SetError(EnvError::UNKNOWN_TOKEN); break;
				case LexerState::STATE_INTEGER:
					if (IsNumber(cc)) LexerChangeState(LexerState::STATE_INTEGER, false, true);
					else if (cc == '.') LexerChangeState(LexerState::STATE_FLOAT, false, true);
					else if (IsOp(cc)) LexerOutput(TokenType::TYPE_INTEGER, LexerState::STATE_OPERATOR, true, true);
					else if (IsWhiteSpace(cc)) LexerOutput(TokenType::TYPE_INTEGER, LexerState::STATE_WS, true, false);
					else SetError(EnvError::UNKNOWN_TOKEN); break;
				case LexerState::STATE_FLOAT:
					if (IsNumber(cc)) LexerChangeState(LexerState::STATE_FLOAT, false, true);
					else if (IsOp(cc)) LexerOutput(TokenType::TYPE_FLOAT, LexerState::STATE_OPERATOR, true, true);
					else if (IsWhiteSpace(cc)) LexerOutput(TokenType::TYPE_FLOAT, LexerState::STATE_WS, true, false);
					else SetError(EnvError::UNKNOWN_TOKEN); break;
				case LexerState::STATE_STRING:
					if (cc == '"') LexerOutput(TokenType::TYPE_STRING, LexerState::STATE_WS, true, false);
					else if (cc == '\\') LexerChangeState(LexerState::STATE_ESCAPE, false, false);
					else if (cc != '\n') LexerChangeState(LexerState::STATE_STRING, false, true);
					else SetError(EnvError::UNKNOWN_TOKEN); break;
				case LexerState::STATE_ESCAPE:
					if (cc == 'f') cc = '\f';
					else if (cc == 'n') cc = '\n';
					else if (cc == 'r') cc = '\r';
					else if (cc == 't') cc = '\t';
					else if (cc == 'v') cc = '\v';
					LexerChangeState(LexerState::STATE_STRING, false, true); break;
				case LexerState::STATE_OPERATOR:
					if (IsNumber(cc)) LexerOutput(TokenType::TYPE_OPERATOR, LexerState::STATE_INTEGER, true, true);
					else if (IsLetter(cc)) LexerOutput(TokenType::TYPE_OPERATOR, LexerState::STATE_IDENT, true, true);
					else if (cc == '.') LexerOutput(TokenType::TYPE_OPERATOR, LexerState::STATE_FLOAT, true, true);
					else if (cc == '"') LexerOutput(TokenType::TYPE_OPERATOR, LexerState::STATE_STRING, true, false);
					else if (IsWhiteSpace(cc)) LexerOutput(TokenType::TYPE_OPERATOR, LexerState::STATE_WS, true, false);
					else if (cc == '=') LexerChangeState(LexerState::STATE_OPERATOR, false, true);
					else if (cc == '/' && lexerCache == "/") LexerChangeState(LexerState::STATE_COMMENT, false, false);
					else if (IsOp(cc)) LexerOutput(TokenType::TYPE_OPERATOR, LexerState::STATE_OPERATOR, true, true);
					else SetError(EnvError::UNKNOWN_TOKEN); break;
				case LexerState::STATE_COMMENT:
					if (cc == '\n') LexerChangeState(LexerState::STATE_WS, true, false);
					else LexerChangeState(LexerState::STATE_COMMENT, false, false); break;
				default:
					SetError(EnvError::UNKNOWN_LEXER_STATE); break;
			}
		}
		////////// Constant & Symbol related //////////
		SymbolInfo CreateFloat(const string& s) {
			double t = std::stod(s);
			if (constNumberCache.find(t) != constNumberCache.end())
				return SymbolInfo{SymbolType::TYPE_CONSTANT, constNumberCache[t]};
			else {
				constNumberList.push_back(t);
				constNumberCache[t] = constNumberList.size() - 1;
				return SymbolInfo{SymbolType::TYPE_CONSTANT, (int)constNumberList.size()-1};
			}
		}
		SymbolInfo CreatePointer(int t) {
			if (t >= 0 && t <= 65535) 
				return SymbolInfo{SymbolType::TYPE_IMMEDIATE, t};
			else if (constNumberCache.find(t) != constNumberCache.end()) 
				return SymbolInfo{SymbolType::TYPE_CONSTANT, constNumberCache[t]};
			else {
				constNumberList.push_back(t);
				constNumberCache[t] = constNumberList.size() - 1;
				return SymbolInfo{SymbolType::TYPE_CONSTANT, (int)constNumberList.size()-1};
			}
		}
		SymbolInfo CreateInteger(const string& s) {
			return CreatePointer(std::stoi(s));
		}
		SymbolInfo CreateString(const string& s) {
			if (s.length() < 32)  { // short string optimization
				if (constStringCache.find(s) != constStringCache.end())
					return CreatePointer(constStringCache[s]);
				else {
					constStringList.push_back(s);
					constStringCache[s] = constStringList.size() - 1;
					return CreatePointer(constStringList.size() - 1);
				}
			}
			else {
				constStringList.push_back(s);
				return CreatePointer(constStringList.size() - 1);
			}
		}
		SymbolInfo FindSymbol(const string& sym_name, int start_level, int end_level)
		{
			for (int i = start_level; i >= end_level; i--)
				if (symbolDicts[i].find(sym_name) != symbolDicts[i].end())
					return symbolDicts[i][sym_name];
			return SymbolInfo{SymbolType::TYPE_UNDEFINED, -1};
		}
		int CreateFunction(const string& s)
		{
			SymbolInfo info = FindSymbol(s, 0, 0);
			if (info.symType == SymbolType::TYPE_FUNCTION) return info.index;
			else {
				funcInfoList.push_back(FuncInfo{0, -1});
				symbolDicts[0][s] = SymbolInfo{SymbolType::TYPE_FUNCTION, (int)funcInfoList.size()-1};
				return funcInfoList.size() - 1;  // funcInfoList index
			}
		}
		int CreateLabel(const string& s)
		{
			SymbolInfo info = FindSymbol(s, 1, 1);
			if (info.symType == SymbolType::TYPE_ALABEL) return info.index;
			else {
				labelList.push_back(-1);
				symbolDicts[1][tokenString + ":"] = SymbolInfo{SymbolType::TYPE_ALABEL, (int)labelList.size()-1};
				return labelList.size() - 1;
			}
		}
		int CreateVar(const string& s) {
			SymbolInfo info = FindSymbol(s, symbolDicts.size()-1, symbolDicts.size()-1);
			if (info.symType == SymbolType::TYPE_VAR) return info.index;
			else {
				int idx = RegAlloc();
				symbolDicts[symbolDicts.size()-1][tokenString] = SymbolInfo{SymbolType::TYPE_VAR, idx};
				return idx;     // register index
			}
		}
		int NewLabel() {
			labelList.push_back(-1);
			return labelList.size() - 1;
		}
		////////// Register related //////////
		int RegAlloc() {
			regCount++;
			if (regCount > regMaxCount)
				regMaxCount = regCount;
			return regCount-1;
		}
		int RegAllocTemp() {
			regTempCount++;
			return RegAlloc();
		}
		void RegFreeTemp(int num) {
			RegFree(num);
			regTempCount -= num;
		}
		bool RegIsTemp(int reg) {
			return (reg >= regCount - regTempCount);
		}
		void RegFree(int num) {
			regCount -= num;
		}
		void RegClear() {
			regCount = 0;
			regMaxCount = 0;
		}
		////////// Inst related //////////
		void InstGenR3(InstOp op, byte a, byte b, byte c) {
			instList.push_back(Inst{op, a, b, c});
		}
		void InstGenRI(InstOp op, byte a, ushort imm16) {
			Inst inst = Inst();
			inst.op = op;
			inst.a = a;
			inst.b = (byte)(imm16 / 256);
			inst.c = (byte)(imm16 % 256);
			instList.push_back(inst);
		}
		void InstGenI2(InstOp op, byte imm8, ushort imm16) {
			Inst inst = Inst();
			inst.op = op;
			inst.a = imm8;
			inst.b = (byte)(imm16 / 256);
			inst.c = (byte)(imm16 % 256);
			instList.push_back(inst);
		}
		void InstGenMov(byte a, byte b) {
			if (a != b) InstGenR3(InstOp::OP_ADD, a, b, 255);
		}
		void InstLabelToOffset(int start_idx, int end_idx) {
			// Convert Label to offset
			for (int i = start_idx; i < end_idx; i++) {
				// Jump Operation(4)
				if (instList[i].op >= InstOp::OP_JNZ && instList[i].op <= InstOp::OP_JDEC) {
					int dst = labelList[(int)instList[i].b * 256 + (int)instList[i].c];
					if (dst < 0) {
						SetError(EnvError::UNDEFINED_LABEL);
						break;
					}
					int delta = dst - i;
					Inst inst = instList[i];
					if (delta >= -32768 && delta < 32768) {
						int offset = delta + 32768;
						inst.b = (byte)(offset / 256);
						inst.c = (byte)(offset % 256);
						instList[i] = inst;
					}
					else {
						SetError(EnvError::JUMP_OUT_OF_RANGE);
						break;
					}
				}
			}
		}
		////////// Parser related //////////
		void ParserChangeState(ParserState new_state, bool stop_parser) {
			parserState = new_state;
			parserStop = stop_parser;
		}
		void ParserCallState(ParserState new_state, ParserState ret_state, bool stop_parser) {
			parserStack.push_back(ParserCallInfo{
				ret_state, //state
				parserData1, //data1
				parserData2, //data2
				parserData3  //data3
			});
			ParserChangeState(new_state, stop_parser);
		}
		void ParserStateReturn(bool stop_parser) {
			ParserCallInfo info = parserStack[parserStack.size()-1];
			parserData1 = info.data1;
			parserData2 = info.data2;
			parserData3 = info.data3;
			ParserChangeState(info.state, stop_parser); 
			parserStack.pop_back();	// stack pop
		}
		int ParserFindLoop() {    // return -1 if not find
			int stateIdx = -1;
			for (int i = parserStack.size() - 1; i >= 0; i--) {
				if (parserStack[i].state == ParserState::STATE_WHILE2 || parserStack[i].state == ParserState::STATE_FOR4) {
					stateIdx = i;
					break;
				}
			}
			return stateIdx;
		}
		void ParserEnterExpr(int p, ParserState ret_state, bool stop_parser) {
			ParserCallState(ParserState::STATE_EXPR1, ret_state, stop_parser);
			parserData1 = p;    // data1 : pcode
			ParserCallState(ParserState::STATE_ATOM1, ParserState::STATE_EXPR1, stop_parser);
		}
		void ParserEnterBlock(bool create_dict, int reg_offset, ParserState ret_state) {
			if (create_dict) symbolDicts.push_back(std::unordered_map<string, SymbolInfo>());
			ParserCallState(ParserState::STATE_BLOCK1, ret_state, true);
			parserData1 = regCount + reg_offset;                // block register count
		}
		void ParserEnterCall(const string& func_name, ParserState ret_state) {
			ParserCallState(ParserState::STATE_CALL1, ret_state, true);
			SymbolInfo info = FindSymbol(func_name, 0, 0);
			if (info.symType == SymbolType::TYPE_UNDEFINED)
				SetError(EnvError::UNDEFINED_SYMBOL);
			else if (info.symType == SymbolType::TYPE_FUNCTION) {
				parserData1 = 0;            // Function argSize
				parserData2 = info.index;   // Function index
			}
			else SetError(EnvError::INVALID_TYPE);
		}
		void ParserCallReturn() {
			RegFreeTemp(parserData1);   // free argSize
			InstGenI2(InstOp::OP_CALL, (byte)regCount, (ushort)parserData2);    // call argSize, func_idx
			parserDataRet = regCount;   // retrun value
			ParserStateReturn(true);
		}
		void ParserExpr1Bop(int p_code) {
			if (parserData1 < (p_code - p_code % 10)) {
				parserData1 = p_code;
				parserData2 = parserDataRet;    // data2 : %left
				ParserEnterExpr(p_code, ParserState::STATE_EXPR2, true);
			}
			else ParserExpr1Retrun(p_code);
		}
		void ParserExpr1Retrun(int p_code) {
			if (RegIsTemp(parserDataRet)) RegFreeTemp(1);
			parserPcodeRet = p_code;
			ParserStateReturn(false);
		}
		void ParserExpr2Bop(InstOp op, int b, int c) {
			if (RegIsTemp(parserData2)) RegFreeTemp(1);
			int regIdx = RegAllocTemp();
			InstGenR3(op, (byte)regIdx, (byte)b, (byte)c);
			parserData2 = regIdx;
		}
		void ParserScan() {
			if (isPanic) return;
			parserStop = false;
			while (!parserStop) {
				switch (parserState) {
					case ParserState::STATE_PROGRAM1:
						if (tokenType == TokenType::TYPE_IDENT) {
							if (tokenString == "using")
								ParserCallState(ParserState::STATE_USING1, ParserState::STATE_PROGRAM1, true);
							else if (tokenString == "function")
								ParserCallState(ParserState::STATE_FUNCTION1, ParserState::STATE_PROGRAM1, true);
							else SetError(EnvError::UNKNOWN_TOKEN);
						}
						else if (tokenType == TokenType::TYPE_EOF)
							ParserChangeState(ParserState::STATE_PROGRAM1, true);
						break;
					case ParserState::STATE_USING1:
						if (tokenType == TokenType::TYPE_IDENT) {
							parserCache = tokenString;
							ParserChangeState(ParserState::STATE_USING2, true);
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_USING2:
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == "=")
							ParserChangeState(ParserState::STATE_USING3, true);
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_USING3:
						if (tokenType == TokenType::TYPE_IDENT) {
							SymbolInfo symInfo = FindSymbol(tokenString, symbolDicts.size() - 1, 0);
							if (symInfo.symType == SymbolType::TYPE_UNDEFINED)
								SetError(EnvError::UNDEFINED_SYMBOL);
							else symbolDicts.back()[parserCache] = symInfo;
						}
						else if (tokenType == TokenType::TYPE_INTEGER) {
							SymbolInfo symInfo = CreateInteger(tokenString);
							symbolDicts.back()[parserCache] = symInfo;
						}
						else if (tokenType == TokenType::TYPE_FLOAT) {
							SymbolInfo symInfo = CreateFloat(tokenString);
							symbolDicts.back()[parserCache] = symInfo;
						}
						else if (tokenType == TokenType::TYPE_STRING) {
							SymbolInfo symInfo = CreateString(tokenString);
							symbolDicts.back()[parserCache] = symInfo;
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						ParserStateReturn(true);
						break;
					case ParserState::STATE_FUNCTION1:
						if (tokenType == TokenType::TYPE_FUNCT) {
							parserData1 = CreateFunction(tokenString); // data1 : funcInfoList index
							if (funcInfoList.size() > 65536) SetError(EnvError::FUNCTION_OVERFLOW);
							symbolDicts.push_back(std::unordered_map<string, SymbolInfo>());
							RegClear();
							labelList.clear();
							labelList.push_back(instList.size());   // function start
							ParserChangeState(ParserState::STATE_FUNCTION2, true);
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_FUNCTION2:
						if (tokenType == TokenType::TYPE_IDENT) {
							CreateVar(tokenString);
							ParserChangeState(ParserState::STATE_FUNCTION3, true);
						}
						else if (tokenType == TokenType::TYPE_OPERATOR && tokenString == ")") {
							ParserChangeState(ParserState::STATE_FUNCTION4, true);
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_FUNCTION3:
						if (tokenType == TokenType::TYPE_OPERATOR) {
							if (tokenString == ",") ParserChangeState(ParserState::STATE_FUNCTION2, true);
							else if (tokenString == ")") ParserChangeState(ParserState::STATE_FUNCTION4, true);
							else SetError(EnvError::UNKNOWN_TOKEN);
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_FUNCTION4:
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == "{")
							ParserEnterBlock(false, -regCount, ParserState::STATE_FUNCTION5);
						else {
							// Clear function-level symbol dictionary
							symbolDicts.pop_back();
							labelList.clear();
							ParserStateReturn(false);
						}
						break;
					case ParserState::STATE_FUNCTION5:
						instList.push_back(Inst{InstOp::OP_RET, 0, 0, 0});   // RET
						funcInfoList[parserData1] = FuncInfo {labelList[0], regMaxCount<=0?1:regMaxCount};
						if (regMaxCount > 255) SetError(EnvError::REGISTER_OVERFLOW);
						else if (constNumberList.size() > 65536) SetError(EnvError::CONSTANT_OVERFLOW);
						else if (labelList.size() > 65536) SetError(EnvError::LABEL_OVERFLOW);
						else InstLabelToOffset(labelList[0], instList.size());
						ParserStateReturn(false);
						labelList.clear();
						break;
					case ParserState::STATE_BLOCK1:
						if (tokenType == TokenType::TYPE_IDENT) {
							if (tokenString == "break") {
								int stateIdx = ParserFindLoop();
								if (stateIdx >= 0) {
									ParserCallInfo info = parserStack[stateIdx];
									// data1 : LOOP_START data2 : LOOP_END
									if (info.state == ParserState::STATE_FOR4) {
										// data2 < 0 : until
										if (info.data2 < 0) InstGenRI(InstOp::OP_JZR, 255, (ushort)(-info.data2));
										// data2 > 0 : to/downto
										else InstGenRI(InstOp::OP_JZR, 255, (ushort)info.data2);
									}
									else InstGenRI(InstOp::OP_JZR, 255, (ushort)info.data2);
								}
								else SetError(EnvError::BREAK_OUTSIDE_LOOP);
								ParserChangeState(ParserState::STATE_BLOCK1, true);
							}
							else if (tokenString == "continue") {
								int stateIdx = ParserFindLoop();
								if (stateIdx >= 0) {
									ParserCallInfo info = parserStack[stateIdx];
									// data1 : LOOP_START data2 : LOOP_END
									if (info.state == ParserState::STATE_FOR4) {
										// data1 < 0 : downto
										if (info.data1 < 0) InstGenRI(InstOp::OP_JDEC, (byte)info.data3, (ushort)(-info.data1));
										// data1 > 0 : to|until
										else InstGenRI(InstOp::OP_JINC, (byte)info.data3, (ushort)info.data1);
									}
									else InstGenRI(InstOp::OP_JZR, 255, (ushort)info.data1);
								}
								else SetError(EnvError::CONTINUE_OUTSIDE_LOOP);
								ParserChangeState(ParserState::STATE_BLOCK1, true);
							}
							else if (tokenString == "using")
								ParserCallState(ParserState::STATE_USING1, ParserState::STATE_BLOCK1, true);
							else if (tokenString == "if") {
								ParserCallState(ParserState::STATE_IF1, ParserState::STATE_BLOCK1, true);
								parserData1 = NewLabel();  // data1 : ELSE
								parserData2 = NewLabel();  // data2 : ENDIF
								ParserEnterExpr(0, ParserState::STATE_IF1, true);
							}
							else if (tokenString == "while") {
								ParserCallState(ParserState::STATE_WHILE1, ParserState::STATE_BLOCK1, true);
								parserData1 = NewLabel();  // data1 : LOOP_START
								parserData2 = NewLabel();  // data2 : LOOP_END
								labelList[parserData1] = instList.size();    // label :LOOP_START
								ParserEnterExpr(0, ParserState::STATE_WHILE1, true);
							}
							else if (tokenString == "for")
								ParserCallState(ParserState::STATE_FOR1, ParserState::STATE_BLOCK1, true);
							else if (tokenString == "return")
								ParserCallState(ParserState::STATE_RETURN1, ParserState::STATE_BLOCK1, true);
							else if (tokenString == "goto")
								ParserCallState(ParserState::STATE_GOTO1, ParserState::STATE_BLOCK1, true);
							else if (tokenString == "var")
								ParserCallState(ParserState::STATE_VAR1, ParserState::STATE_BLOCK1, true);
							else {
								ParserCallState(ParserState::STATE_ASSIGN1, ParserState::STATE_BLOCK1, true);
								SymbolInfo info = FindSymbol(tokenString, symbolDicts.size() - 1, 0);
								if (info.symType == SymbolType::TYPE_UNDEFINED)
									SetError(EnvError::UNDEFINED_SYMBOL);
								else if (info.symType == SymbolType::TYPE_VAR)
									parserData1 = info.index; // data1 : dst reg
								else SetError(EnvError::INVALID_TYPE);
							}
						}
						else if (tokenType == TokenType::TYPE_LABEL) {
							int idx = CreateLabel(tokenString + ":");
							labelList[idx] = instList.size();
							ParserChangeState(ParserState::STATE_BLOCK1, true);
						}
						else if (tokenType == TokenType::TYPE_FUNCT)
							ParserEnterCall(tokenString, ParserState::STATE_BLOCK1);
						else if (tokenType == TokenType::TYPE_OPERATOR) {
							if (tokenString == "{")
								ParserEnterBlock(true, 0, ParserState::STATE_BLOCK1);
							else if (tokenString == "}") {
								RegFree(regCount - parserData1);
								symbolDicts.pop_back();
								ParserStateReturn(true);
							}
							else SetError(EnvError::UNKNOWN_TOKEN);
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_ASSIGN1:
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == "=")
							ParserEnterExpr(0, ParserState::STATE_ASSIGN2, true);
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_ASSIGN2:
						InstGenMov((byte)parserData1, (byte)parserDataRet);
						ParserStateReturn(false);
						break;
					case ParserState::STATE_VAR1:
						if (tokenType == TokenType::TYPE_IDENT) {
							ParserChangeState(ParserState::STATE_ASSIGN1, true);
							parserData1 = CreateVar(tokenString); // data1 : dst reg
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_IF1:
						InstGenRI(InstOp::OP_JZR, (byte)parserDataRet, (byte)parserData1); // jzr %condition, :ELSE
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == "{")
							ParserEnterBlock(true, 0, ParserState::STATE_IF2);
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_IF2:
						InstGenRI(InstOp::OP_JZR, 255, (byte)parserData2); // jmp :ENDIF
						labelList[parserData1] = instList.size(); // label :ELSE
						if (tokenType == TokenType::TYPE_IDENT && tokenString == "else")
							ParserChangeState(ParserState::STATE_IF3, true);
						else ParserChangeState(ParserState::STATE_IF4, false);
						break;
					case ParserState::STATE_IF3:
						if (tokenType == TokenType::TYPE_IDENT && tokenString == "if") {
							ParserCallState(ParserState::STATE_IF1, ParserState::STATE_IF4, true);
							parserData1 = NewLabel();  // data1 : ELSE data2 : ENDIF
							ParserEnterExpr(0, ParserState::STATE_IF1, true);
						}
						else if (tokenType == TokenType::TYPE_OPERATOR && tokenString == "{") {
							ParserEnterBlock(true, 0, ParserState::STATE_IF4);
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_IF4:
						labelList[parserData2] = instList.size(); // label :ENDIF
						ParserStateReturn(false);
						break;
					case ParserState::STATE_WHILE1:
						InstGenRI(InstOp::OP_JZR, (byte)parserDataRet, (ushort)parserData2); // jzr %condition, :LOOP_END
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == "{")
							ParserEnterBlock(true, 0, ParserState::STATE_WHILE2);
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_WHILE2:
						InstGenRI(InstOp::OP_JZR, 255, (ushort)parserData1); // jmp :LOOP_START
						labelList[parserData2] = instList.size(); // label :LOOP_END
						ParserStateReturn(false);
						break;
					case ParserState::STATE_FOR1:
						symbolDicts.push_back(std::unordered_map<string, SymbolInfo>());
						if (tokenType == TokenType::TYPE_IDENT) {
							int regIdx = CreateVar(tokenString);
							RegAlloc();                        // reg2  : %compare
							parserData1 = NewLabel();          // data1 : LOOP_START
							parserData2 = NewLabel();          // data2 : LOOP_END
							parserData3 = regIdx;              // data3 : %count
							ParserCallState(ParserState::STATE_ASSIGN1, ParserState::STATE_FOR2, true);
							parserData1 = regIdx;              // data1 : %count
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_FOR2:
						if (tokenType == TokenType::TYPE_IDENT) {
							labelList[parserData1] = instList.size();
							if (tokenString == "to")
							{ }                             // data1 > 0 data2 > 0
							else if (tokenString == "downto")
							{ parserData1 = -parserData1; } // data1 < 0 data2 > 0
							else if (tokenString == "until")
							{ parserData2 = -parserData2; } // data1 > 0 data2 < 0
							else SetError(EnvError::UNKNOWN_TOKEN);
							ParserEnterExpr(0, ParserState::STATE_FOR3, true);
						}
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_FOR3:
						InstGenMov((byte)(parserData3 + 1), (byte)parserDataRet); // mov %compare %compare_val
						if (parserData1 < 0) {   // downto
							labelList[-parserData1] = instList.size(); // label :LOOP_START
							byte regTemp = (byte)(RegAlloc());
							InstGenR3(InstOp::OP_SGE, regTemp, (byte)parserData3, (byte)(parserData3 + 1));    // sge %temp %count %compare
							InstGenRI(InstOp::OP_JZR, regTemp, (ushort)parserData2);    // jzr %temp :LOOP_END
							RegFree(1);
						}
						else if (parserData2 < 0) {  // until
							labelList[parserData1] = instList.size(); // label :LOOP_START
							byte regTemp = (byte)(RegAlloc());
							InstGenR3(InstOp::OP_SLT, regTemp, (byte)parserData3, (byte)(parserData3 + 1));    // slt %temp %count %compare
							InstGenRI(InstOp::OP_JZR, regTemp, (ushort)(-parserData2));    // jzr %temp :LOOP_END
							RegFree(1);
						}
						else { // to
							labelList[parserData1] = instList.size(); // label :LOOP_START
							byte regTemp = (byte)(RegAlloc());
							InstGenR3(InstOp::OP_SGE, regTemp, (byte)(parserData3 + 1), (byte)parserData3);    // sle %temp %count %compare
							InstGenRI(InstOp::OP_JZR, regTemp, (ushort)parserData2);    // jzr %temp :LOOP_END
							RegFree(1);
						}
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == "{")
							ParserEnterBlock(false, -2, ParserState::STATE_FOR4);
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_FOR4:
						// data1 < 0 : downto
						if (parserData1 < 0) InstGenRI(InstOp::OP_JDEC, (byte)parserData3, (ushort)(-parserData1));
						// data1 > 0 : to|until
						else InstGenRI(InstOp::OP_JINC, (byte)parserData3, (ushort)parserData1);
						labelList[parserData2 < 0 ? -parserData2 : parserData2] = instList.size(); // label :LOOP_END
						ParserStateReturn(false);
						break;
					case ParserState::STATE_RETURN1:
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == "}") {
							InstGenI2(InstOp::OP_RET, 0, 0);    // ret
							ParserStateReturn(false);
						}
						else ParserEnterExpr(0, ParserState::STATE_RETURN2, false);
						break;
					case ParserState::STATE_RETURN2:
						InstGenMov(0, (byte)parserDataRet); // mov %ret %ret_val
						InstGenI2(InstOp::OP_RET, 0, 0);    // ret
						ParserStateReturn(false);
						break;
					case ParserState::STATE_GOTO1: 
						{ 
							int idx = CreateLabel(tokenString + ":");
							InstGenRI(InstOp::OP_JZR, 255, (ushort)idx);
							ParserStateReturn(true);
						}
						break;
					case ParserState::STATE_CALL1:
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == ")")
							ParserCallReturn();
						else ParserEnterExpr(0, ParserState::STATE_CALL2, false);
						break;

					case ParserState::STATE_CALL2: 
						{
							int idx = RegAllocTemp();
							InstGenMov((byte)idx, (byte)parserDataRet); // mov %argx %argx_val
							parserData1++;
						}
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == ")")
							ParserCallReturn();
						else if (tokenType == TokenType::TYPE_OPERATOR && tokenString == ",")
							ParserChangeState(ParserState::STATE_CALL1, true);
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_ATOM1:
						if (tokenType == TokenType::TYPE_OPERATOR) {
							if (tokenString == "(") ParserEnterExpr(0, ParserState::STATE_ATOM2, true);
							else if (tokenString == "-" || tokenString == "!") {
								parserData1 = (tokenString == "-") ? 60 : 61;   // data1 : pcode
								ParserCallState(ParserState::STATE_ATOM1, ParserState::STATE_ATOM3, true);
							}
							else SetError(EnvError::UNKNOWN_TOKEN);
						}
						else if (tokenType == TokenType::TYPE_FUNCT)
							ParserEnterCall(tokenString, ParserState::STATE_ATOM4);
						else {
							SymbolInfo symInfo = SymbolInfo{SymbolType::TYPE_IMMEDIATE, 0};
							if (tokenType == TokenType::TYPE_INTEGER) symInfo = CreateInteger(tokenString);
							else if (tokenType == TokenType::TYPE_FLOAT) symInfo = CreateFloat(tokenString);
							else if (tokenType == TokenType::TYPE_STRING) symInfo = CreateString(tokenString);
							else if (tokenType == TokenType::TYPE_IDENT) symInfo = FindSymbol(tokenString, symbolDicts.size() - 1, 0);
							else SetError(EnvError::UNKNOWN_TOKEN);
							if (symInfo.symType == SymbolType::TYPE_UNDEFINED) SetError(EnvError::UNDEFINED_SYMBOL);
							else if (symInfo.symType == SymbolType::TYPE_VAR) parserDataRet = symInfo.index;
							else if (symInfo.symType == SymbolType::TYPE_IMMEDIATE) {
								int idx = RegAllocTemp();
								InstGenRI(InstOp::OP_LDI, (byte)idx, (ushort)symInfo.index);    // ldi %temp #imm
								parserDataRet = idx;
							}
							else if (symInfo.symType == SymbolType::TYPE_CONSTANT) {
								int idx = RegAllocTemp();
								InstGenRI(InstOp::OP_LDC, (byte)idx, (ushort)symInfo.index);    // ldc %temp const_idx
								parserDataRet = idx;
							}
							else SetError(EnvError::INVALID_TYPE);
							ParserStateReturn(true);
						}
						break;
					case ParserState::STATE_ATOM2:
						if (tokenType == TokenType::TYPE_OPERATOR && tokenString == ")")
							ParserStateReturn(true);
						else SetError(EnvError::UNKNOWN_TOKEN);
						break;
					case ParserState::STATE_ATOM3:
						if (parserData1 == 60) {  // -     sub %dst zero %src
							if (RegIsTemp(parserDataRet))
								InstGenR3(InstOp::OP_SUB, (byte)parserDataRet, 255, (byte)parserDataRet);
							else {
								int idx = RegAllocTemp();
								InstGenR3(InstOp::OP_SUB, (byte)idx, 255, (byte)parserDataRet);
								parserDataRet = idx;
							}
						}
						else { // !    seq %dst zero %src
							if (RegIsTemp(parserDataRet))
								InstGenR3(InstOp::OP_SEQ, (byte)parserDataRet, 255, (byte)parserDataRet);
							else {
								int idx = RegAllocTemp();
								InstGenR3(InstOp::OP_SEQ, (byte)idx, 255, (byte)parserDataRet);
								parserDataRet = idx;
							}
						}
						ParserStateReturn(false);
						break;
					case ParserState::STATE_ATOM4:
						{
							int idx = RegAllocTemp();   // %ret
							parserDataRet = idx;
						}
						ParserStateReturn(false);
						break;
					case ParserState::STATE_EXPR1:
						if (tokenType == TokenType::TYPE_OPERATOR) {
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
						else if (tokenType == TokenType::TYPE_IDENT) {
							if (tokenString == "and") {
								if (parserData1 < 20) {
									parserData2 = parserDataRet;
									if (!RegIsTemp(parserDataRet)) {
										parserData2 = RegAllocTemp();
										InstGenMov((byte)parserData2, (byte)parserDataRet); // mov %result %left
									}
									parserData3 = NewLabel();   // data3 : :skip
									InstGenRI(InstOp::OP_JZR, (byte)parserData2, (ushort)parserData3);  // jzr %result :skip
									parserData1 = 20;
									ParserEnterExpr(20, ParserState::STATE_EXPR2, true);
								}
								else ParserExpr1Retrun(20);
							}
							else if (tokenString == "or") {
								if (parserData1 < 10) {
									parserData2 = parserDataRet;
									if (!RegIsTemp(parserDataRet)) {
										parserData2 = RegAllocTemp();
										InstGenMov((byte)parserData2, (byte)parserDataRet); // mov %result %left
									}
									parserData3 = NewLabel();   // data3 : :skip
									InstGenRI(InstOp::OP_JNZ, (byte)parserData2, (ushort)parserData3);  // jnz %result :skip
									parserData1 = 10;
									ParserEnterExpr(10, ParserState::STATE_EXPR2, true);
								}
								else ParserExpr1Retrun(10);
							}
							else ParserExpr1Retrun(0);
						}
						else ParserExpr1Retrun(0);
						break;
					case ParserState::STATE_EXPR2:
						if (parserData1 == 50) ParserExpr2Bop(InstOp::OP_MUL, parserData2, parserDataRet); // mul
						else if (parserData1 == 51) ParserExpr2Bop(InstOp::OP_DIV, parserData2, parserDataRet); // div
						else if (parserData1 == 40) ParserExpr2Bop(InstOp::OP_ADD, parserData2, parserDataRet); // add
						else if (parserData1 == 41) ParserExpr2Bop(InstOp::OP_SUB, parserData2, parserDataRet); // sub
						else if (parserData1 == 30) ParserExpr2Bop(InstOp::OP_SEQ, parserData2, parserDataRet); // ==
						else if (parserData1 == 31) ParserExpr2Bop(InstOp::OP_SNE, parserData2, parserDataRet); // !=
						else if (parserData1 == 32) ParserExpr2Bop(InstOp::OP_SLT, parserDataRet, parserData2); // >
						else if (parserData1 == 33) ParserExpr2Bop(InstOp::OP_SLT, parserData2, parserDataRet); // <
						else if (parserData1 == 34) ParserExpr2Bop(InstOp::OP_SGE, parserData2, parserDataRet); // >=
						else if (parserData1 == 35) ParserExpr2Bop(InstOp::OP_SGE, parserDataRet, parserData2); // <=
						else if (parserData1 == 20 || parserData1 == 10) {
							InstGenMov((byte)parserData2, (byte)parserDataRet);
							labelList[parserData3] = instList.size();
						}
						else SetError(EnvError::UNKNOWN_PCODE);
						if (parserPcodeRet > 0) {
							if (parserPcodeRet == 20) {
								int regLeft = parserData2;
								if (!RegIsTemp(regLeft)) {
									parserData2 = RegAllocTemp();
									InstGenMov((byte)parserData2, (byte)regLeft); // mov %result %left
								}
								parserData3 = NewLabel();   // data3 : :skip
								InstGenRI(InstOp::OP_JZR, (byte)parserData2, (ushort)parserData3);  // jzr %result :skip
							}
							else if (parserPcodeRet == 10) {
								int regLeft = parserData2;
								if (!RegIsTemp(regLeft)) {
									parserData2 = RegAllocTemp();
									InstGenMov((byte)parserData2, (byte)regLeft); // mov %result %left
								}
								parserData3 = NewLabel();   // data3 : :skip
								InstGenRI(InstOp::OP_JNZ, (byte)parserData2, (ushort)parserData3);  // jnz %result :skip
							}
							parserData1 = parserPcodeRet;
							ParserEnterExpr(parserData1, ParserState::STATE_EXPR2, true);
						}
						else {
							if (RegIsTemp(parserData2)) RegFreeTemp(1);
							parserDataRet = parserData2;
							ParserStateReturn(false);
						}
						break;
					default: SetError(EnvError::UNKNOWN_PARSER_STATE); break;
				}
			}
		}
	};
	class CoidState {
	public:	
		////////// Environment //////////
		CoidEnv* env = nullptr;
		////////// Variable //////////
		bool isFinished = true;
		bool isWaiting = false;
		bool isPanic = false;
		StateError stateError = StateError::NO_STATE_ERROR;
		int callCode = 0;    // >= 0
		// Stack
		double* dataStack = nullptr;
		CallInfo* callStack = nullptr;
		// State Register
		int pc = 0;    //program counter
		int fp = 0;    //main frame pointer
		int csp = 0;   //call stack pointer
		int callSaveReg = 0;
		// Limits
		int maxDataStackSize = 0;
		int maxCallStackSize = 0;
		////////// Functions //////////
		CoidState(CoidEnv* coid_env, int max_datastack_size, int max_callstack_size) {
			env = coid_env;
			maxDataStackSize = max_datastack_size;
			maxCallStackSize = max_callstack_size;
			dataStack = new double[maxDataStackSize];
			callStack = new CallInfo[maxCallStackSize];
		}
		~CoidState() {
			delete[] dataStack;
			delete[] callStack;
		}
		void SetEntry(const string& user_func_name, std::initializer_list<double> args) {
			Clear();
			SymbolInfo info = env->FindGlobalSymbol(user_func_name);
			if (info.symType != SymbolType::TYPE_FUNCTION) {
				SetError(StateError::UNDEFINED_FUNCTION);
				return;
			}
			CallFunc(info.index, 0);
			isFinished = false;
			int i = 0; for (double ele : args) {
				dataStack[i] = ele;
				i++;
			} 
		}
		void SetRet(double val) {
			if (!isWaiting) return;
			dataStack[fp] = val;
			fp -= callSaveReg;
			isWaiting = false;
		}
		double GetArg(int index) {
			if ( fp + index >= maxDataStackSize) {
				SetError(StateError::INDEX_OUT_OF_BOUNDS);
				return 0;
			}
			else return dataStack[fp + index];
		}
		string GetArgString(int index) {
			int idx = (int)GetArg(index);
			if (idx < 0 || idx >= env->constStringList.size()) {
				SetError(StateError::INDEX_OUT_OF_BOUNDS);
				return "";
			}
			else return env->constStringList[idx];
		}		
		double GetRet() { return GetArg(0); }
		string GetRetString() { return GetArgString(0); }
		void Clear() {
			pc = 0;
			fp = 0;
			csp = 0;
			isFinished = true;
			isWaiting = false;
			isPanic = false;
			stateError = StateError::NO_STATE_ERROR;
			callCode = 0;
			callSaveReg = 0;
		}
		void Execute() {
			if (isFinished || isWaiting || isPanic) return;
			Inst inst = env->instList[pc];
			if (inst.op < InstOp::OP_JNZ) {
				double b = inst.b == 255 ? 0 : dataStack[fp + inst.b];
				double c = inst.c == 255 ? 0 : dataStack[fp + inst.c];
				switch (inst.op) {
					case InstOp::OP_ADD:    // ADD | 0 | A | B | C | type-R3
						dataStack[fp + inst.a] = b + c; break;
					case InstOp::OP_SUB:    // SUB | 1 | A | B | C | type-R3 
						dataStack[fp + inst.a] = b - c; break;
					case InstOp::OP_MUL:    // MUL | 2 | A | B | C | type-R3
						dataStack[fp + inst.a] = b * c; break;
					case InstOp::OP_DIV:    // DIV | 3 | A | B | C | type-R3
						dataStack[fp + inst.a] = b / c; break;
					case InstOp::OP_SLT:    // SLT | 4 | A | B | C | type-R3
						dataStack[fp + inst.a] = (b < c) ? 1 : 0; break;
					case InstOp::OP_SGE:    // SGE | 5 | A | B | C | type-R3
						dataStack[fp + inst.a] = (b >= c) ? 1 : 0; break;
					case InstOp::OP_SEQ:    // SEQ | 6 | A | B | C | type-R3
						dataStack[fp + inst.a] = (b == c) ? 1 : 0; break;
					case InstOp::OP_SNE:    // SNE | 7 | A | B | C | type-R3
						dataStack[fp + inst.a] = (b != c) ? 1 : 0; break;
					default:
						SetError(StateError::UNKNOWN_INSTRUCTION); break;
				}
				pc += 1;
			}
			else if (inst.op < InstOp::OP_CALL) {
				double a = inst.a == 255 ? 0 : dataStack[fp + inst.a];
				int imm = (int)inst.b * 256 + (int)inst.c;
				switch (inst.op) {
					case InstOp::OP_JNZ:    // JNZ | 8 | A |uint16 | type-RI
						if (a != 0) pc += imm - 32768; 
						else pc += 1; 
						break;
					case InstOp::OP_JZR:    // JZR | 9 | A |uint16 | type-RI
						if (a == 0) pc += imm - 32768;
						else pc += 1; 
						break;
					case InstOp::OP_JINC:   // JINC|10 | A |uint16 | type-RI
						dataStack[fp + inst.a] += 1; 
						pc += imm - 32768; 
						break;
					case InstOp::OP_JDEC:   // JDEC|11 | A |uint16 | type-RI
						dataStack[fp + inst.a] -= 1;
						pc += imm - 32768;
						break;
					default:
						SetError(StateError::UNKNOWN_INSTRUCTION); break;
				}
			}
			else {
				int imm = (int)inst.b * 256 + (int)inst.c;
				switch (inst.op) {
					case InstOp::OP_CALL:   // CALL|12 |u8 |uint16 | type-I2
						CallFunc(imm, inst.a); break;
					case InstOp::OP_RET:    // RET |13 | 0 |   0   | type-I2
						csp -= 1;
						fp = callStack[csp].savedFp;
						pc = callStack[csp].savedPc;
						if (csp == 0) isFinished = true;
						break;
					case InstOp::OP_LDC:   // LDC |14 | A |uint16 | type-RI
						dataStack[fp + inst.a] = env->constNumberList[imm];
						pc += 1; 
						break;
					case InstOp::OP_LDI:   // LDI |15 | A |uint16 | type-RI
						dataStack[fp + inst.a] = imm;
						pc += 1; 
						break;
					default:
						SetError(StateError::UNKNOWN_INSTRUCTION); break;
				}
			}
		}
	private:
		////////// Error Handling //////////
		void SetError(StateError err_code) {
			stateError = err_code;
			isPanic = true;
		}
		////////// Runtime //////////
		void CallFunc(int func_idx, int save_reg) {
			FuncInfo info = env->funcInfoList[func_idx];
			if (info.regSize < 0) {
				SetError(StateError::UNDEFINED_FUNCTION);
				return;
			}
			if (fp + save_reg + info.regSize > maxDataStackSize) {
				SetError(StateError::DATASTACK_OVERFLOW);
				return;
			}
			if (info.codeAddr >= 0) {
				if (csp + 1 > maxCallStackSize) {
					SetError(StateError::CALLSTACK_OVERFLOW);
					return;
				}
				callStack[csp].savedFp = fp;
				callStack[csp].savedPc = pc + 1;
				csp += 1;
				fp += save_reg;
				pc = info.codeAddr;
			}
			else {
				callCode = -info.codeAddr - 1;    //call system function
				callSaveReg = save_reg;
				fp += save_reg;
				pc += 1;
				isWaiting = true;
			}
		}
	};
}

#endif
