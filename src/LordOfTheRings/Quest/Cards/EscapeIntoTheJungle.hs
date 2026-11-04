module LordOfTheRings.Quest.Cards.EscapeIntoTheJungle (escapeIntoTheJungle) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype EscapeIntoTheJungle = EscapeIntoTheJungle QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeIntoTheJungle :: QuestCard EscapeIntoTheJungle
escapeIntoTheJungle = quest EscapeIntoTheJungle Cards.escapeIntoTheJungle

chieftain :: EnemyMatcher
chieftain = EnemyWithCardCode (toCardCode Enemies.urukChieftain)

-- | Uruk Chieftain cannot take non-combat damage.
-- Each non-unique Orc enemy gets -10 engagement cost.
-- The players cannot defeat this stage while Uruk Chieftain is in play. If the players
-- defeat this stage, they win the game (it is the last stage).
instance HasModifiersFor EscapeIntoTheJungle where
  getModifiersFor (EscapeIntoTheJungle a) = when a.flipped do
    chieftains <- select chieftain
    for_ chieftains \e -> modified a e [CannotTakeNonCombatDamage]
    orcs <- select (EnemyWithTrait Orc <> not_ UniqueEnemy)
    for_ orcs \e -> modified a e [EngagementCostModifier (-10)]
    unless (null chieftains) $ modifySelf a [CannotBeDefeated]

-- | Forced: After Uruk Chieftain engages a player, it makes an immediate attack.
instance HasAbilities EscapeIntoTheJungle where
  getAbilities (EscapeIntoTheJungle a) = [mkAbility a 1 $ forced $ MatchingEnemyEngagedWindow After chieftain | a.flipped]

-- | When Revealed: Add Uruk Chieftain to the staging area. Shuffle the encounter discard
-- pile into the encounter deck and reveal 1 encounter card per player.
instance RunMessage EscapeIntoTheJungle where
  runMessage msg q@(EscapeIntoTheJungle attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      EscapeIntoTheJungle <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.urukChieftain)
      shuffleEncounterDiscardIntoDeck AnyCard
      players <- getRemainingPlayers
      revealEncounterCards (length players)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [(pid, eid) | Window _ (EnemyEngaged eid pid) <- windows] \(pid, eid) -> immediateAttack pid eid
      pure q
    _ -> EscapeIntoTheJungle <$> runMessage msg attrs
