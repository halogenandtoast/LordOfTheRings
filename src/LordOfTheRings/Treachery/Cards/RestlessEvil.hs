module LordOfTheRings.Treachery.Cards.RestlessEvil (restlessEvil) where

import LordOfTheRings.Trait (Trait (Undead))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RestlessEvil = RestlessEvil TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

restlessEvil :: TreacheryCard RestlessEvil
restlessEvil = treachery RestlessEvil Cards.restlessEvil

-- | When Revealed: Each Undead enemy gets +1 threat, +1 attack, and +1 defense until the
-- end of the round. If there are no Undead enemies in play, Restless Evil gains surge.
-- Shadow: Each Undead enemy engaged with you gets +1 attack and +1 defense until the end
-- of the round.
instance RunMessage RestlessEvil where
  runMessage msg t@(RestlessEvil attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      undead <- select (EnemyWithTrait Undead)
      for_ undead \e -> roundModifiers attrs e [ThreatModifier 1, AttackModifier 1, DefenseModifier 1]
      when (null undead) revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      undead <- select (EnemyWithTrait Undead <> EnemyEngagedWith (PlayerWithId pid))
      for_ undead \e -> roundModifiers attrs e [AttackModifier 1, DefenseModifier 1]
      pure t
    _ -> RestlessEvil <$> runMessage msg attrs
