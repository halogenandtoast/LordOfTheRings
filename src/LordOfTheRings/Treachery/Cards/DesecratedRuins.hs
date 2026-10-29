module LordOfTheRings.Treachery.Cards.DesecratedRuins (desecratedRuins) where

import LordOfTheRings.Card (CardCost (StaticCost), CardDef (..), toCardDef)
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (placeDamageOnActiveLocation)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DesecratedRuins = DesecratedRuins TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

desecratedRuins :: TreacheryCard DesecratedRuins
desecratedRuins = treachery DesecratedRuins Cards.desecratedRuins

-- | When Revealed: Place 3 damage on the active location. If there is no active location,
-- Desecrated Ruins gains Surge.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys an ally, place X damage on
-- the active location. X is that ally's cost. Once the attack is over the shadow reads, for
-- each ally it could have destroyed, whether it has left play, its cost noted beforehand.
-- ponytail: an ally the attack could destroy that left play during it some other way counts
-- as destroyed, as Morgul Vale's shadow reads it.
instance RunMessage DesecratedRuins where
  runMessage msg t@(DesecratedRuins attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- selectAny IsActiveLocation
      if active then placeDamageOnActiveLocation attrs 3 else revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      exposed <- if null defenders then charactersControlledBy pid else pure defenders
      allies <- for [aid | AllyTarget aid <- exposed] \aid -> (aid,) . cost . toCardDef <$> getAlly aid
      afterAttack eid [IfAnyLeftPlay [AllyTarget aid] [DoStep x (ResolveShadowEffect cid eid pid [])] | (aid, x) <- allies, x > 0]
      pure t
    DoStep x (ResolveShadowEffect cid _ _ []) | cid == attrs.cardId -> do
      placeDamageOnActiveLocation (CardIdSource cid) x
      pure t
    _ -> DesecratedRuins <$> runMessage msg attrs
   where
    cost def = case cdCost def of
      Just (StaticCost n) -> n
      _ -> 0
