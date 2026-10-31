module LordOfTheRings.Scenario.Scenarios.ShadowAndFlame (shadowAndFlame) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithTrait), cardMatches)
import LordOfTheRings.Modifier (ModifierType (HeroCommittedThisRound))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers (isDurinsBane)
import LordOfTheRings.Source (Source (GameSource))
import LordOfTheRings.Trait (Trait (Attack))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype ShadowAndFlame = ShadowAndFlame ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowAndFlame :: Scenario
shadowAndFlame =
  Scenario
    $ scenario (ShadowAndFlame . (nightmareDeckL ?~ nightmare)) "shadow-and-flame" "Shadow and Flame"
 where
  nightmare =
    nightmareDeck
      Nightmare.shadowAndFlameNightmare
      EncounterSet.ShadowAndFlameNightmare
      [ (Enemies.strayGoblin, 3)
      , (Locations.theMountainsRoots, 3)
      , (Enemies.goblinScout, 3)
      , (Locations.goblinTunnels, 2)
      , (Enemies.rangingGoblin, 3)
      , (Treacheries.fierySword, 1)
      , (Treacheries.manyThongedWhip, 1)
      ]

-- | The Balrog deck and its discard pile, in nightmare mode.
data Meta = Meta {balrogDeck :: [EncounterCard], balrogDiscard :: [EncounterCard]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta [] []) $ parseMaybe parseJSON attrs.meta

isBalrogCard :: EncounterCard -> Bool
isBalrogCard = cardMatches (CardWithTrait Attack)

{- | Nightmare: "Setup: Shuffle the 10 encounter cards with the Attack trait into a separate
'Balrog deck.' When Durin's Bane would be dealt a shadow card, deal it from the Balrog deck
instead of the encounter deck. Shadow cards dealt from the Balrog deck are discarded into a
separate Balrog discard pile. At the beginning of each combat phase (or if the Balrog deck
runs out of cards), shuffle the Balrog discard pile back into the Balrog deck."
-}
instance RunMessage ShadowAndFlame where
  runMessage msg (ShadowAndFlame attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.ShadowAndFlame, EncounterSet.DeepsOfMoria, EncounterSet.GoblinsOfTheDeep]
      let (balrog, encounterDeck) = partition isBalrogCard cards
      deck <- shuffleM balrog
      firstStage <- genEncounterCard Quests.nearingTheGate
      laterStages <- traverse genEncounterCard [Quests.theRearGuard, Quests.lastLordOfMoria]
      putQuestIntoPlay firstStage
      pure
        $ ShadowAndFlame
        $ attrs
        & (encounterDeckL .~ encounterDeck)
        & (questDeckL .~ laterStages)
        & (metaL .~ toJSON (Meta deck []))
    DealShadowCard eid | attrs.nightmare -> do
      bane <- isDurinsBane eid
      if bane
        then do
          (deck, discards) <-
            if null meta.balrogDeck
              then (,[]) <$> shuffleM meta.balrogDiscard
              else pure (meta.balrogDeck, meta.balrogDiscard)
          case deck of
            card : rest -> do
              push $ AttachShadowCard eid card
              pure $ withMeta meta {balrogDeck = rest, balrogDiscard = discards}
            [] -> pure $ ShadowAndFlame attrs
        else ShadowAndFlame <$> runMessage msg attrs
    AddToEncounterDiscard cards | attrs.nightmare, any isBalrogCard cards -> do
      let (balrog, rest) = partition isBalrogCard cards
      attrs' <- runMessage (AddToEncounterDiscard rest) attrs
      pure $ ShadowAndFlame $ attrs' & metaL .~ toJSON meta {balrogDiscard = balrog <> meta.balrogDiscard}
    BeginPhase CombatPhase | attrs.nightmare -> do
      deck <- shuffleM (meta.balrogDeck <> meta.balrogDiscard)
      ShadowAndFlame <$> runMessage msg (attrs & metaL .~ toJSON (Meta deck []))
    -- a hero committed this round, as The Rear Guard reads, even one since removed
    CommitToQuest (HeroTarget _) -> do
      roundModifier GameSource GameTarget HeroCommittedThisRound
      ShadowAndFlame <$> runMessage msg attrs
    _ -> ShadowAndFlame <$> runMessage msg attrs
   where
    meta = getMeta attrs
    withMeta m = ShadowAndFlame $ attrs & metaL .~ toJSON m
