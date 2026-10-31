module LordOfTheRings.Quest.Cards.AmbushInTheNight (ambushInTheNight) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (..), CardType (EnemyType), toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (AttachedTo, InPlayArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RoadToRivendell.Helpers (getArwen)

newtype AmbushInTheNight = AmbushInTheNight QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Road to Rivendell's nightmare stage 2, in place of Orc Outpost.
ambushInTheNight :: QuestCard AmbushInTheNight
ambushInTheNight = quest AmbushInTheNight Cards.ambushInTheNight

goblinGates :: LocationMatcher
goblinGates = LocationWithCardCode (toCardCode Locations.goblinGate)

-- | Goblin Gate gets +1 quest point for each enemy engaged with a player.
instance HasModifiersFor AmbushInTheNight where
  getModifiersFor (AmbushInTheNight a) = when a.flipped do
    engaged <- length <$> select (EnemyEngagedWith AnyPlayer)
    gates <- select goblinGates
    when (engaged > 0) $ for_ gates \g -> modified a g [QuestPointModifier engaged]

{- | Side A, When Revealed: Search the encounter deck, discard pile and victory display for
Goblin Gate and add it to the staging area, if able. Shuffle the encounter discard pile into
the encounter deck. Discard cards from the top of the encounter deck until X enemies have
been discarded, where X is the number of players in the game. Reveal all enemies discarded
by this effect.

Side B, When Revealed: The first player loses control of Arwen Undómiel and attaches her to
Goblin Gate (she is now guarded by Goblin Gate). While she is guarded, the first player
cannot gain control of Arwen Undómiel. Progress cannot be placed on this stage unless the
players control Arwen Undómiel.
-}
instance RunMessage AmbushInTheNight where
  runMessage msg q@(AmbushInTheNight attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let isGate = cardMatches (cardIs Locations.goblinGate)
      case ( find isGate scenario.encounterDeck
           , find isGate scenario.encounterDiscard
           , find isGate [c | EncounterCard c <- scenario.victoryDisplay]
           ) of
        (Just gate, _, _) -> removeFromEncounterDeck gate >> addToStaging gate
        (_, Just gate, _) -> addToStagingFromDiscard gate
        (_, _, Just gate) -> removeFromVictoryDisplay (EncounterCard gate) >> addToStaging gate
        _ -> pure ()
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      askToResolveWhenRevealed attrs
      AmbushInTheNight <$> runMessage msg attrs
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      x <- length <$> getRemainingPlayers
      let isEnemy c = cdCardType (toCardDef c) == EnemyType
          untilEnemies _ [] = []
          untilEnemies k (c : cs)
            | k <= 0 = []
            | otherwise = c : untilEnemies (if isEnemy c then k - 1 else k) cs
          discarded = untilEnemies x deck
      discardTopOfEncounterDeck attrs (length discarded)
      traverse_ revealFromEncounterDiscard (filter isEnemy discarded)
      pure q
    WhenRevealed (isTarget attrs -> True) -> do
      arwen <- getArwen
      gate <- select goblinGates
      for_ (take 1 gate) \g -> for_ arwen \a -> placeAlly a (AttachedTo $ toTarget g)
      pure q
    -- as Goblin Gate is explored she is no longer guarded, and the first player takes her
    -- back
    CheckWindows ws | notNull [lid | Window When (Explored lid) <- ws] -> do
      gates <- select goblinGates
      for_ [lid | Window When (Explored lid) <- ws, lid `elem` gates] \lid -> do
        arwen <- traverse getAlly =<< getArwen
        lead <- getLead
        for_ arwen \a -> when (a.placement == AttachedTo (toTarget lid)) $ placeAlly a.id (InPlayArea lead)
      AmbushInTheNight <$> runMessage msg attrs
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      arwen <- filterM (\a -> selectAny (AllyWithId a <> AllyControlledBy AnyPlayer)) =<< getArwen
      if notNull arwen then AmbushInTheNight <$> runMessage msg attrs else pure q
    _ -> AmbushInTheNight <$> runMessage msg attrs
