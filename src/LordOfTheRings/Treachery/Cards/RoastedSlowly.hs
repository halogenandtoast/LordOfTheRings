module LordOfTheRings.Treachery.Cards.RoastedSlowly (roastedSlowly) where

import LordOfTheRings.Card (CardDef (..), toCardCode, toCardDef)
import LordOfTheRings.Trait (Trait (Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RoastedSlowly = RoastedSlowly TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

roastedSlowly :: TreacheryCard RoastedSlowly
roastedSlowly = treachery RoastedSlowly Cards.roastedSlowly

-- | When Revealed: Destroy all heroes with the card "Sacked!" attached. Then, shuffle
-- Roasted Slowly back into the encounter deck.
-- Shadow: If attacking enemy is a Troll, remove 2 damage tokens from it.
instance RunMessage RoastedSlowly where
  runMessage msg t@(RoastedSlowly attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      sacked <- hostsOfTreachery (toCardCode Cards.sacked)
      for_ [h | h@(HeroTarget _) <- sacked] (push . Destroy)
      push $ ShuffleIntoEncounterDeck (toTarget attrs)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemy <- getEnemy eid
      when (Troll `member` cdCardTraits (toCardDef enemy) && enemy.damage > 0)
        $ removeTokens attrs eid Damage (min 2 enemy.damage)
      pure t
    _ -> RoastedSlowly <$> runMessage msg attrs
