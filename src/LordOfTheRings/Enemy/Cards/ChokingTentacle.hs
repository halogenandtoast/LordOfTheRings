module LordOfTheRings.Enemy.Cards.ChokingTentacle (chokingTentacle) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (tentacleStrikes)

newtype ChokingTentacle = ChokingTentacle EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

chokingTentacle :: EnemyCard ChokingTentacle
chokingTentacle = enemy ChokingTentacle Cards.chokingTentacle

-- | Forced: After resolving an attack made by Choking Tentacle, discard the top card of
-- the encounter deck. If that card has a shadow effect or is a Tentacle enemy, attach
-- Choking Tentacle to a character the defending player controls as a Tentacle attachment
-- with the text: "Forced: After attached character readies, deal it 1 damage."
instance HasAbilities ChokingTentacle where
  getAbilities (ChokingTentacle a) = case a.placement of
    AttachedTo host -> [mkAbility a 2 $ forced $ BecameReadyWindow After host]
    _ -> [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage ChokingTentacle where
  runMessage msg e@(ChokingTentacle attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> discardTopOfEncounterDeckFor attrs (PlayerTarget pid) 1
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget pid) cards | tentacleStrikes cards -> do
      characters <- charactersControlledBy pid
      chooseOrRunOneM pid do
        questionLabeled "chokingTentacle.attach"
        causedBy attrs
        targets characters \c -> placeEnemy attrs.id (AttachedTo c)
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      withHost attrs \host -> dealDamage attrs host 1
      pure e
    _ -> ChokingTentacle <$> runMessage msg attrs
