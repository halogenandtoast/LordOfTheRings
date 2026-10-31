module LordOfTheRings.Treachery.Cards.RottenStench (rottenStench) where

import LordOfTheRings.Card (CardDef (..), CardType (LocationType), toCardDef)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RottenStench = RottenStench TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rottenStench :: TreacheryCard RottenStench
rottenStench = treachery RottenStench Cards.rottenStench

-- | When Revealed: Return the topmost location in the encounter discard pile to the staging
-- area. Until the end of the phase, each location in the staging area gets +1 threat.
-- Shadow: Deal 1 damage to the defending character.
instance RunMessage RottenStench where
  runMessage msg t@(RottenStench attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardPile <- getEncounterDiscard
      for_ (find ((== LocationType) . cdCardType . toCardDef) discardPile) addToStagingFromDiscard
      phaseModifier attrs GameTarget (EachLocationInStagingArea $ ThreatModifier 1)
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure t
    _ -> RottenStench <$> runMessage msg attrs
