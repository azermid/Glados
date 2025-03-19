{-# LANGUAGE FlexibleInstances #-}
module VM.Instruction where

import MLL.Def
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS


-- Instruction : Représente une action à exécuter dans le programme
data Instruction = LoadConst Expr      -- Charger une constante
                 | StoreVar Text   -- Stocker une valeur dans une variable
                 | LoadVar Text    -- Charger une valeur depuis une variable
                 | BinaryOp Op       -- Opération binaire
                 | UnaryOp UOp       -- Opération unaire
                 | Return            -- Retourner la dernière valeur
                 | Label Text        -- Label
                 | Jmp Jmp
                 | Cmp RelOp         -- Comparison
                 deriving (Eq)

data Jmp = JmpAny Text
        | JmpIf Text
        | JmpIfNot Text
        | JmpF Text
        | JmpC Text -- Jmp in closure case
        deriving (Eq)

-- Show instance

instance Show Jmp where
    show (JmpAny l)   = "JmpAny " ++ show l
    show (JmpIf l)    = "JmpIf " ++ show l
    show (JmpIfNot l) = "JmpIfNot " ++ show l
    show (JmpF l)     = "JmpF " ++ show l
    show (JmpC l)     = "JmpC " ++ show l

instance Show Instruction where
    show (LoadConst i) = "LoadConst " ++ show i
    show (LoadVar v)   = "LoadVar " ++ show v
    show (StoreVar v)  = "StoreVar " ++ show v
    show (UnaryOp op)  = "UnaryOp " ++ show op
    show (BinaryOp op) = "BinaryOp " ++ show op
    show (Cmp op)      = "RelOp " ++ show op
    show (Jmp lbl)     = show lbl
    show (Label l)     = "\nLabel " ++ show l
    show Return        = "Return "

-- Helper function to show a list of instructions
showInstructions :: [Instruction] -> String
showInstructions instrs = unlines (map show instrs)

-- Bytecode translation
instance Binary Instruction where
    put (LoadConst expr) = putWord8 0 >> put expr
    put (StoreVar name)  = putWord8 1 >> putText name
    put (LoadVar name)   = putWord8 2 >> putText name
    put (BinaryOp op)    = putWord8 3 >> put op
    put (UnaryOp op)     = putWord8 4 >> put op
    put Return           = putWord8 5
    put (Label name)     = putWord8 6 >> putText name
    put (Jmp j)          = putWord8 7 >> put j
    put (Cmp op)         = putWord8 8 >> put op

    get = do
        tag <- getWord8
        case tag of
            0 -> LoadConst <$> get
            1 -> StoreVar <$> getText
            2 -> LoadVar <$> getText
            3 -> BinaryOp <$> get
            4 -> UnaryOp <$> get
            5 -> return Return
            6 -> Label <$> getText
            7 -> Jmp <$> get
            8 -> Cmp <$> get
            
            tah -> error $ "Invalid bytecode: " ++ show tah

instance Binary Jmp where
    put (JmpAny name)   = putWord8 0 >> putText name
    put (JmpIf name)    = putWord8 1 >> putText name
    put (JmpIfNot name) = putWord8 2 >> putText name
    put (JmpF name)     = putWord8 3 >> putText name
    put (JmpC name)     = putWord8 4 >> putText name

    get = do
        tag <- getWord8
        case tag of
            0 -> JmpAny <$> getText
            1 -> JmpIf <$> getText 
            2 -> JmpIfNot <$> getText
            3 -> JmpF <$> getText
            4 -> JmpC <$> getText
            _ -> error "Invalid bytecode"

instance Binary Op where
    put Add    = putWord8 0
    put Sub    = putWord8 1
    put Mul    = putWord8 2
    put Div    = putWord8 3
    put AddEq  = putWord8 4
    put SubEq  = putWord8 5
    put MulEq  = putWord8 6
    put DivEq  = putWord8 7
    put AddOne = putWord8 8
    put SubOne = putWord8 9

    get = do
        tag <- getWord8
        case tag of
            0 -> return Add
            1 -> return Sub
            2 -> return Mul
            3 -> return Div
            4 -> return AddEq
            5 -> return SubEq
            6 -> return MulEq
            7 -> return DivEq
            8 -> return AddOne
            9 -> return SubOne
            _ -> error "Invalid bytecode"

instance Binary UOp where
    put Neg = putWord8 0
    put Not = putWord8 1

    get = do
        tag <- getWord8
        case tag of
            0 -> return Neg
            1 -> return Not
            _ -> error "Invalid bytecode"

instance Binary RelOp where
    put Eq  = putWord8 0
    put Neq = putWord8 1
    put Lt  = putWord8 2
    put Le  = putWord8 3
    put Gt  = putWord8 4
    put Ge  = putWord8 5

    get = do
        tag <- getWord8
        case tag of
            0 -> return Eq
            1 -> return Neq
            2 -> return Lt
            3 -> return Le
            4 -> return Gt
            5 -> return Ge
            _ -> error "Invalid bytecode"

instance Binary Type where
    put IntType    = putWord8 0
    put BoolType   = putWord8 1
    put CharType   = putWord8 2
    put StringType = putWord8 3
    put VoidType   = putWord8 4
    put FloatType  = putWord8 5

    get = do
        tag <- getWord8
        case tag of
            0 -> return IntType
            1 -> return BoolType
            2 -> return CharType
            3 -> return StringType
            4 -> return VoidType
            5 -> return FloatType
            _ -> error "Invalid bytecode"

instance Binary Expr where
    put (Lit n)                 = putWord8 0 >> putInt32le (fromIntegral n)
    put (StrLit s)              = putWord8 1 >> putText s
    put (BoolLit b)             = putWord8 2 >> putBool b
    put (CharLit c)             = putWord8 3 >> putWord8 (fromIntegral $ fromEnum c)
    put (FloatLit f)            = putWord8 4 >> putFloatle f
    put (Var name)              = putWord8 5 >> putText name
    put (Unary op expr)         = putWord8 6 >> put op >> put expr
    put (Binary op expr1 expr2) = putWord8 7 >> put op >> put expr1 >> put expr2
    put Null                    = putWord8 8
    put (Call name args)        = putWord8 9 >> putText name >> put args
    put (Rel op expr1 expr2)    = putWord8 10 >> put op >> put expr1 >> put expr2
    put (Bind (Bind' t name))   = putWord8 11 >> put t >> putText name

    get = do
        tag <- getWord8
        case tag of
            0  -> Lit <$> fromIntegral <$> getInt32le
            1  -> StrLit <$> getText
            2  -> BoolLit <$> getBool
            3  -> CharLit . toEnum . fromIntegral <$> getWord8
            4  -> FloatLit <$> getFloatle
            5  -> Var <$> getText
            6  -> Unary <$> get <*> get
            7  -> Binary <$> get <*> get <*> get
            8  -> return Null
            9  -> Call <$> getText <*> get
            10 -> Rel <$> get <*> get <*> get
            11 -> Bind <$> (Bind' <$> get <*> getText)
            _  -> error "Invalid bytecode"

putBool :: Bool -> Put
putBool True = putWord8 1
putBool False = putWord8 0

getBool :: Get Bool
getBool = do
    tag <- getWord8
    case tag of
        0 -> return False
        1 -> return True
        _ -> error "Invalid bytecode"

putText :: Text -> Put
putText txt = do
    let bs = encodeUtf8 txt
    putInt32le (fromIntegral $ BS.length bs)
    putByteString bs

getText :: Get Text
getText = do
    len <- getInt32le
    bs  <- getByteString (fromIntegral len)
    return $ decodeUtf8 bs


serializeBytecode :: [Instruction] -> BL.ByteString
serializeBytecode = encode

deserializeBytecode :: BL.ByteString -> [Instruction]
deserializeBytecode = decode