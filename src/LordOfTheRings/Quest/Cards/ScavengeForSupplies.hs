module LordOfTheRings.Quest.Cards.ScavengeForSupplies (scavengeForSupplies) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Player.Runner (canPlayAtNoCost)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (isBlank)
import LordOfTheRings.Trait (Trait (Troll))

newtype ScavengeForSupplies = ScavengeForSupplies QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
scavengeForSupplies :: QuestCard ScavengeForSupplies
scavengeForSupplies = quest ScavengeForSupplies Cards.scavengeForSupplies

-- | The cost to play each player card is increased by 1.
instance HasModifiersFor ScavengeForSupplies where
  getModifiersFor (ScavengeForSupplies a) = modified a GameTarget [IncreaseCost AnyCard 1]

-- | Forced: When this stage is defeated, search the encounter deck and discard pile for a
-- Troll enemy and put it into play engaged with the player with the highest threat. Each
-- player may play a card from his hand, at no cost.
-- Of players tied for the highest threat, the first player chooses.
instance RunMessage ScavengeForSupplies where
  runMessage msg (ScavengeForSupplies attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      unlessM (isBlank attrs) do
        lead <- getLead
        players <- getRemainingPlayers
        threats <- traverse (fmap (.threat) . getPlayer) players
        let highest = maximumMay threats
            tied = [p | (p, t) <- zip players threats, Just t == highest]
        chooseOrRunOneM lead do
          questionLabeled "scavengeForSupplies.engage"
          causedBy attrs
          targets tied \pid -> searchEncounterDeckAndDiscardToEngage pid attrs troll
        eachPlayer \pid -> do
          hand <- filterM canPlayAtNoCost . (.hand) =<< getPlayer pid
          unless (null hand) $ chooseOneM pid do
            questionLabeled "scavengeForSupplies.play"
            causedBy attrs
            targets hand \c -> playFromHandAtNoCost pid c.id
            labeled "scavengeForSupplies.none" (pure ())
      ScavengeForSupplies <$> runMessage msg attrs
    _ -> ScavengeForSupplies <$> runMessage msg attrs
   where
    troll = CardMatchAll [CardWithType EnemyType, CardWithTrait Troll]
