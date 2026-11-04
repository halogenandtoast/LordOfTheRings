module LordOfTheRings.Ally.Cards.Gavin (gavin) where

import LordOfTheRings.Ally.Fleet (runShip)
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (genEncounterCard, toCardCode)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gamePhase, gameScenario)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype Gavin = Gavin AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Dream-chaser campaign. Surge. Ranged.
gavin :: AllyCard Gavin
gavin = ally Gavin Cards.gavin

{- | Planning Action: Shuffle Gavin and 1 random set-aside Send for Help into the encounter
deck.
When Revealed: Put Gavin into play under any player's control committed to the quest. Add
1 resource to Lanwyn's resource pool, if able.
Revealed outside the quest phase there is no quest to commit him to, so he only comes into
play.
-}
instance HasAbilities Gavin where
  getAbilities (Gavin a) =
    [ restricted a 1 (DuringPhase PlanningPhase) $ ActionAbility Free
    , mkAbility a 2 $ silent $ entersPlay a
    ]

sendsForHelp :: [CardCode]
sendsForHelp = map toCardCode [Treacheries.sendForHelpLeadership, Treacheries.sendForHelpTactics, Treacheries.sendForHelpSpirit, Treacheries.sendForHelpLore]

-- | When Gavin leaves play (other than by his ability), remove him from the game.
instance RunMessage Gavin where
  runMessage msg s@(Gavin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a.id
      putOnTopOfEncounterDeck =<< genEncounterCard Cards.gavin
      helps <- getsGame (filter ((`elem` sendsForHelp) . toCardCode) . scenarioSetAside . toAttrs . gameScenario)
      unless (null helps) do
        help <- uniform helps
        push $ ShuffleSetAsideIntoEncounterDeck [help]
      shuffleEncounterDeck
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      phase <- getsGame gamePhase
      when (phase == QuestPhase) $ commitToQuest a.id
      lanwyns <- select (HeroWithTitle "Lanwyn")
      for_ lanwyns \h -> placeResources a h 1
      pure s
    _ -> Gavin <$> runShip (pure ()) msg a
