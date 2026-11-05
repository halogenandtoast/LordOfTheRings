module LordOfTheRings.Enemy.Cards.SarumanTheRoadToIsengard (sarumanTheRoadToIsengard) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (hasWizardry, wizardry)

newtype SarumanTheRoadToIsengard = SarumanTheRoadToIsengard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Indestructible, as its card def says.
sarumanTheRoadToIsengard :: EnemyCard SarumanTheRoadToIsengard
sarumanTheRoadToIsengard = enemy SarumanTheRoadToIsengard Cards.sarumanTheRoadToIsengard

-- | Immune to player card effects. Cannot leave the staging area.
instance HasModifiersFor SarumanTheRoadToIsengard where
  getModifiersFor (SarumanTheRoadToIsengard a) =
    modifySelf a [ImmuneToPlayerCardEffects, CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer]

{- | Forced: When Saruman is dealt a shadow card with a Wizardry effect, the defending player
resolves that Wizardry effect as if he is the only player in the game.

Saruman attacks only from the staging area: as stage 3B has him engaged with the first
player, or as Saruman's Staff has him attack. Either way his shadow cards are dealt to him
right before the attack they are for, with nothing between, so the attack names the
defending player as it begins: each Wizardry card not yet turned faceup resolves as he is
declared as an attacker, before defenders are declared.
-}
instance RunMessage SarumanTheRoadToIsengard where
  runMessage msg (SarumanTheRoadToIsengard attrs) = case msg of
    CheckWindows ws | pid : _ <- [p | Window When (EnemyAttacking eid p) <- ws, eid == attrs.id] -> do
      let dealt = [c | c <- attrs.shadowCards, c.id `notElem` enemyShadowRevealed attrs, hasWizardry (toCardCode c)]
      for_ dealt \c -> wizardry attrs (toCardCode c) [pid]
      SarumanTheRoadToIsengard <$> runMessage msg attrs
    _ -> SarumanTheRoadToIsengard <$> runMessage msg attrs
