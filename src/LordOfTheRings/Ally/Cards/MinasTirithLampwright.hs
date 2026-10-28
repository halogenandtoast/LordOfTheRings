module LordOfTheRings.Ally.Cards.MinasTirithLampwright (minasTirithLampwright) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType, LocationType, TreacheryType))

newtype MinasTirithLampwright = MinasTirithLampwright AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

minasTirithLampwright :: AllyCard MinasTirithLampwright
minasTirithLampwright = ally MinasTirithLampwright Cards.minasTirithLampwright

-- | Response: After an encounter card with surge is revealed, discard Minas Tirith
-- Lampwright to name enemy, location, or treachery. If the next encounter card revealed
-- is the named type, discard it without resolving its effects.
instance HasAbilities MinasTirithLampwright where
  getAbilities (MinasTirithLampwright a) = [mkAbility a 1 $ response $ SurgeCardRevealedWindow After]

instance RunMessage MinasTirithLampwright where
  runMessage msg s@(MinasTirithLampwright a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discard a
      -- ponytail: the effect waits for the next card until the end of the round; the
      -- surge that triggered it reveals that card at once
      let name t = roundModifier a GameTarget (DiscardNextRevealed t)
      chooseOneM pid do
        questionLabeled "minasTirithLampwright.name"
        causedBy a
        labeled "minasTirithLampwright.enemy" $ name EnemyType
        labeled "minasTirithLampwright.location" $ name LocationType
        labeled "minasTirithLampwright.treachery" $ name TreacheryType
      pure s
    _ -> MinasTirithLampwright <$> runMessage msg a
