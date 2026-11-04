module LordOfTheRings.Treachery.Cards.TrollAttack (trollAttack) where

import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TrollAttack = TrollAttack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

trollAttack :: TreacheryCard TrollAttack
trollAttack = treachery TrollAttack Cards.trollAttack

-- | When Revealed: All engaged Troll enemies attack. If no Troll enemies are engaged, Troll
-- Attack gains surge.
-- Shadow: If attacking enemy is a Troll, resolve this attack against each player. (Attack
-- is undefended against each player not engaged with this enemy.)
instance RunMessage TrollAttack where
  runMessage msg t@(TrollAttack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      trolls <- traverse getEnemy =<< select (EnemyWithTrait Troll)
      let engaged = [(p, toId e) | e <- trolls, EngagedWith p <- [e.placement]]
      if null engaged
        then revealEncounterCard
        else for_ engaged \(p, eid) -> immediateAttack p eid
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      troll <- selectAny (EnemyWithId eid <> EnemyWithTrait Troll)
      others <- filter (/= pid) <$> getRemainingPlayers
      when troll $ afterAttack eid [UndefendedDamage p eid | p <- others]
      pure t
    _ -> TrollAttack <$> runMessage msg attrs
