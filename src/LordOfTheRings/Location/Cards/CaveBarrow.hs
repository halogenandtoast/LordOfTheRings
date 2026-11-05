module LordOfTheRings.Location.Cards.CaveBarrow (caveBarrow) where

import LordOfTheRings.Card.CardType (CardType (AllyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype CaveBarrow = CaveBarrow LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

caveBarrow :: LocationCard CaveBarrow
caveBarrow = location CaveBarrow Cards.caveBarrow

-- | The players, as a group, cannot have more than 5 allies in play: no ally can be played
-- while there are 5.
instance HasModifiersFor CaveBarrow where
  getModifiersFor (CaveBarrow a) = do
    allies <- length <$> select AnyAlly
    when (allies >= 5) $ modified a GameTarget [CannotPlay (CardWithType AllyType)]

-- | (If there are more than 5 allies in play, discard allies until only 5 remain.) Read as
-- an ally enters play, and as Cave Barrow does.
instance HasAbilities CaveBarrow where
  getAbilities (CaveBarrow a) =
    [ mkAbility a 1 $ silent $ AllyEnteredPlayWindow After Nothing Nothing
    , mkAbility a 2 $ silent $ CardEntersStagingAreaWindow After (toTarget a)
    ]

instance RunMessage CaveBarrow where
  runMessage msg l@(CaveBarrow a) = case msg of
    UseCardAbility _ (isSource a -> True) _ _ -> do
      discardAnAllyOverFive msg
      pure l
    DoStep 1 inner@(UseCardAbility _ (isSource a -> True) _ _) -> do
      discardAnAllyOverFive inner
      pure l
    _ -> CaveBarrow <$> runMessage msg a
   where
    -- the first player discards one at a time until only 5 remain
    discardAnAllyOverFive again = do
      allies <- select AnyAlly
      when (length allies > 5) do
        lead <- getLead
        chooseOneM lead do
          questionLabeled "caveBarrow.discard"
          causedBy a
          targets allies \ally -> do
            discard a ally
            doStep 1 again
