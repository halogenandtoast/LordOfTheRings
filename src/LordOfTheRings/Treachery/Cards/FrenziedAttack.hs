module LordOfTheRings.Treachery.Cards.FrenziedAttack (frenziedAttack) where

import LordOfTheRings.Card.CardType (CardType (AllyType))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FrenziedAttack = FrenziedAttack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frenziedAttack :: TreacheryCard FrenziedAttack
frenziedAttack = treachery FrenziedAttack Cards.frenziedAttack

-- | When Revealed: Each player draws a card. Each player discards each ally from his
-- hand. If no allies were discarded this way, Frenzied Attack gains surge.
-- Shadow: Until the end of the round, attacking enemy cannot take damage.
instance RunMessage FrenziedAttack where
  runMessage msg t@(FrenziedAttack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> drawCards p 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      hands <- for players \p -> (p,) . filter ((== AllyType) . (.cardType)) . (.hand) <$> getPlayer p
      for_ hands \(p, allies) -> traverse_ (discardFromHand p) allies
      when (all (null . snd) hands) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifier attrs eid CannotTakeDamage
      pure t
    _ -> FrenziedAttack <$> runMessage msg attrs
