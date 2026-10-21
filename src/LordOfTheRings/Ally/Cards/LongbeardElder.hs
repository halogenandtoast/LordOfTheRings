module LordOfTheRings.Ally.Cards.LongbeardElder (longbeardElder) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (LocationType), cdCardType, toCardDef)

newtype LongbeardElder = LongbeardElder AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

longbeardElder :: AllyCard LongbeardElder
longbeardElder = ally LongbeardElder Cards.longbeardElder

-- | Response: After Longbeard Elder commits to a quest, look at the top card of the
-- encounter deck. If that card is a location, place 1 progress token on the current
-- quest. Otherwise, Longbeard Elder gets -1 willpower until the end of the phase.
instance HasAbilities LongbeardElder where
  getAbilities (LongbeardElder a) = [mkAbility a 1 $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage LongbeardElder where
  runMessage msg s@(LongbeardElder a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lookAtTopOfEncounterDeck pid a False
      getTopOfEncounterDeck >>= \case
        Just top | cdCardType (toCardDef top) == LocationType -> placeProgressOnCurrentQuest a 1
        _ -> phaseModifier a a (WillpowerModifier (-1))
      pure s
    _ -> LongbeardElder <$> runMessage msg a
