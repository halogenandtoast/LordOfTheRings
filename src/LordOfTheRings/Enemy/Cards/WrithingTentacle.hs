module LordOfTheRings.Enemy.Cards.WrithingTentacle (writhingTentacle) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Placement (Placement (AttachedTo, EngagedWith))
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (tentacleStrikes)
import LordOfTheRings.Trait (Trait (Tentacle))

newtype WrithingTentacle = WrithingTentacle EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

writhingTentacle :: EnemyCard WrithingTentacle
writhingTentacle = enemy WrithingTentacle Cards.writhingTentacle

-- | As a Tentacle attachment: "Attached character cannot commit to the quest."
instance HasModifiersFor WrithingTentacle where
  getModifiersFor (WrithingTentacle a) = withHost a \host -> modified a host [CannotCommitToQuest]

-- | Forced: When Writhing Tentacle engages a player, discard the top card of the encounter
-- deck. If that card has a shadow effect or is a Tentacle enemy, Writhing Tentacle makes an
-- immediate attack, then attaches to the defending character as a Tentacle attachment.
instance HasAbilities WrithingTentacle where
  getAbilities (WrithingTentacle a) = case a.placement of
    AttachedTo _ -> []
    _ -> [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | The immediate attack is marked, for the 'AttackDefended' that ends it, by a trait it
-- gains for that attack.
marker :: ModifierType
marker = GainTrait Tentacle

instance RunMessage WrithingTentacle where
  runMessage msg e@(WrithingTentacle attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards | tentacleStrikes cards -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> do
        attackModifier attrs attrs.id marker
        immediateAttack pid attrs.id
      pure e
    AttackDefended _ eid (defender : _) | eid == attrs.id -> do
      struck <- elem marker <$> getModifiers attrs.id
      when struck $ placeEnemy attrs.id (AttachedTo defender)
      pure e
    _ -> WrithingTentacle <$> runMessage msg attrs
