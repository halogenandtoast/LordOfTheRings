module LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth (theRingGoesSouth) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..), updateLog)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher (cardIs, cardMatches, getLead, getRemainingPlayers)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheRingGoesSouth = TheRingGoesSouth ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRingGoesSouth :: Scenario
theRingGoesSouth = Scenario $ scenario TheRingGoesSouth "the-ring-goes-south" "The Ring Goes South"

{- | The Ring Goes South encounter set makes up the encounter deck (The Road Darkens rules
insert). Stage 1A sets Lust for the Ring, Redhorn Pass, Doors of Durin and Watcher in the
Water aside: Lust for the Ring is a burden, in no encounter set, so it is made for the
purpose.
-}
instance RunMessage TheRingGoesSouth where
  runMessage msg (TheRingGoesSouth attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheRingGoesSouth]
      lust <- genEncounterCard Treacheries.lustForTheRing
      let aside c = any (`cardMatches` c) [cardIs Locations.redhornPass, cardIs Locations.doorsOfDurinTheRingGoesSouth, cardIs Enemies.watcherInTheWater]
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.theCouncilOfElrond
      laterStages <- traverse genEncounterCard [Quests.theNineWalkers, Quests.theHuntIsUp, Quests.theGatesOfMoria]
      putQuestIntoPlay firstStage
      pure
        $ TheRingGoesSouth
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ lust : setAside'
        & questDeckL .~ laterStages
    -- Campaign: Setup: Each player may change hero cards he controls without incurring the
    -- +1 threat penalty (see 'freeHeroChange'). The players have earned the boon cards:
    -- Sting, Mithril Shirt, Glamdring, and Andúril. Each player chooses one and adds it to
    -- his hand. If any of those boon cards remain unchosen, shuffle them into the first
    -- player's deck. Each joins the Campaign Pool for the deck it went to.
    DoStep 9 Setup | isJust attrs.campaign -> do
      chooseBoons =<< getRemainingPlayers
      TheRingGoesSouth <$> runMessage msg attrs
    -- Resolution: If Lust for the Ring is attached to a hero, the players have earned that
    -- burden.
    ResolveCampaign | Just l <- attrs.campaign -> do
      attached <- getsGame (any (\t -> toCardCode t == toCardCode Treacheries.lustForTheRing && isHero t.placement) . toList . (.entities.treacheries))
      let earned = PoolCard (toCardCode Treacheries.lustForTheRing) Nothing Nothing
          l' = if attached then updateLog (AddToPool earned) l else l
      TheRingGoesSouth <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> TheRingGoesSouth <$> runMessage msg attrs
   where
    isHero = \case
      AttachedTo (HeroTarget _) -> True
      _ -> False
    boons = [Attachments.stingTheRoadDarkens, Attachments.mithrilShirtTheRoadDarkens, Attachments.glamdringTheRoadDarkens, Attachments.andurilTheRoadDarkens]
    chooseBoons :: ReverseQueue m => [PlayerId] -> m ()
    chooseBoons = go boons
     where
      go :: ReverseQueue n => [CardDef] -> [PlayerId] -> n ()
      go left = \case
        [] -> do
          lead <- getLead
          seat <- seatOf lead
          for_ left \def -> do
            card <- genPlayerCard def (Just lead)
            shuffleIntoDeck lead card
            updateCampaignLog $ AddToPool $ PoolCard (toCardCode def) seat Nothing
        p : ps -> do
          cards <- traverse (`genPlayerCard` Just p) left
          seat <- seatOf p
          chooseOrRunOneM p do
            questionLabeled "theRingGoesSouth.chooseBoon"
            showing (map PlayerCard cards)
            for_ (zip left cards) \(def, c) -> targeting (CardIdTarget c.id) do
              putInHand c
              updateCampaignLog $ AddToPool $ PoolCard (toCardCode def) seat Nothing
              go (filter ((/= toCardCode def) . toCardCode) left) ps
