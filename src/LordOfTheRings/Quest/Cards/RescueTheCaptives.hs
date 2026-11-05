module LordOfTheRings.Quest.Cards.RescueTheCaptives (rescueTheCaptives) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getPursuit, reducePursuit)
import LordOfTheRings.Trait (Trait (UrukHai))

newtype RescueTheCaptives = RescueTheCaptives QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

rescueTheCaptives :: QuestCard RescueTheCaptives
rescueTheCaptives = quest RescueTheCaptives Cards.rescueTheCaptives

-- | X is the pursuit value (1 printed, see 'Cards.rescueTheCaptives').
instance HasModifiersFor RescueTheCaptives where
  getModifiersFor (RescueTheCaptives a) = do
    pursuit <- getPursuit
    modifySelf a [QuestPointModifier (pursuit - 1)]

-- | Response: After an enemy is defeated, reduce the pursuit value by 1.
instance HasAbilities RescueTheCaptives where
  getAbilities (RescueTheCaptives a) = [mkAbility a 1 $ response $ EnemyDefeatedWindow After | a.flipped]

{- | When Revealed: Each player searches the encounter deck and discard pile for an Uruk-hai
enemy, reveals it, and adds it to the staging area. Shuffle the encounter deck. Each player
reveals his as its revealer, "you" on it reading him.

The players cannot defeat this stage while Uglúk is in play. If the players defeat this
stage, they have rescued each Captive hero and won the game.
-}
instance RunMessage RescueTheCaptives where
  runMessage msg q@(RescueTheCaptives attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      RescueTheCaptives <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> asRevealer p $ searchEncounterDeckAndDiscardToReveal p attrs (CardWithType EnemyType <> CardWithTrait UrukHai)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      reducePursuit 1
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      pure q
    _ -> RescueTheCaptives <$> runBlockedStage (EnemyWithCardCode $ toCardCode Enemies.ugluk) msg attrs
