module LordOfTheRings.Enemy.Cards.TheWitchKing (theWitchKing, theWitchKingSorcerer) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase, QuestPhase))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (discardUntilEnemyToStaging)

-- | The Witch-king of The Battle of the Pelennor Fields, his Captain side or his Sorcerer
-- side: stages 3A and 5A put him into play on one or the other.
newtype TheWitchKing = TheWitchKing EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

theWitchKing :: EnemyCard TheWitchKing
theWitchKing = enemy TheWitchKing Cards.theWitchKing

theWitchKingSorcerer :: EnemyCard TheWitchKing
theWitchKingSorcerer = enemy TheWitchKing Cards.theWitchKingSorcerer

sorcerer :: EnemyAttrs -> Bool
sorcerer a = toCardCode a == toCardCode Cards.theWitchKingSorcerer

{- | Captain: Immune to player card effects. Cannot be optionally engaged or take damage.
Sorcerer: Only the engaged player can declare attackers against The Witch-king.
-}
instance HasModifiersFor TheWitchKing where
  getModifiersFor (TheWitchKing a)
    | sorcerer a = modifySelf a [OnlyEngagedPlayerFights]
    | otherwise = modifySelf a [ImmuneToPlayerCardEffects, CannotBeOptionallyEngagedBy AnyPlayer, CannotTakeDamage]

{- | Captain: Forced: At the beginning of the quest phase, discard cards from the encounter
deck until an enemy is discarded. Add the discarded enemy to the staging area.
Sorcerer: Forced: At the beginning of the encounter phase, The Witch-king engages the first
player. Then, it makes an immediate attack.
-}
instance HasAbilities TheWitchKing where
  getAbilities (TheWitchKing a)
    | sorcerer a = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]
    | otherwise = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage TheWitchKing where
  runMessage msg e@(TheWitchKing attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _
      | sorcerer attrs -> do
          lead <- getLead
          engageEnemy lead attrs.id
          immediateAttack lead attrs.id
          pure e
      | otherwise -> do
          discardUntilEnemyToStaging attrs
          pure e
    _ -> TheWitchKing <$> runMessage msg attrs
