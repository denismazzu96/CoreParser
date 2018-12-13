import TastyInjection
import BasicSpec
import SequencingSpec
import ChoicesSpec
import DerPrimSpec
import TokenSpec
import AlterSpec
import VarSpec
import AExprSpec
import ExprSpec
import PrecedenceSpec
import ProgramSpec

main :: IO ()
main = do
  defaultMain (testGroup "" tests)
    where
      tests =
        BasicSpec.tests ++
        SequencingSpec.tests ++
        ChoicesSpec.tests ++
        DerPrimSpec.tests ++
        TokenSpec.tests ++
        AlterSpec.tests ++
        VarSpec.tests ++
        AExprSpec.tests ++
        ExprSpec.tests ++
        PrecedenceSpec.tests ++
        ProgramSpec.tests