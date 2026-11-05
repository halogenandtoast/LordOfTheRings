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

"Dealt a shadow card with ..." is checked as the card turns faceup in the attack's shadow
step, after defenders are declared: one discarded before then never triggers it (Nate
French, ALeP 3.003; Caleb, ALeP 3.274 #3).
-}
instance RunMessage SarumanTheRoadToIsengard where
  runMessage msg (SarumanTheRoadToIsengard attrs) = case msg of
    CheckWindows ws -> do
      for_ [(c, pid) | (c, pid, _) <- revealedShadowCards attrs ws, hasWizardry (toCardCode c)] \(c, pid) ->
        wizardry attrs (toCardCode c) [pid]
      SarumanTheRoadToIsengard <$> runMessage msg attrs
    _ -> SarumanTheRoadToIsengard <$> runMessage msg attrs
