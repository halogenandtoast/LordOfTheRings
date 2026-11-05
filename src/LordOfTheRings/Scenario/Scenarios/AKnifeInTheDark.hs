module LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark (aKnifeInTheDark) where

import Data.List (elemIndex)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches, getHero, select)
import LordOfTheRings.Matcher.Types (CardMatcher (CardMatchAny), EnemyMatcher (AnyEnemy), HeroMatcher (HeroControlledBy), PlayerMatcher (PlayerWithId))
import LordOfTheRings.Modifier (ModifierType (ThreatModifier))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype AKnifeInTheDark = AKnifeInTheDark ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aKnifeInTheDark :: Scenario
aKnifeInTheDark = Scenario $ scenario AKnifeInTheDark "a-knife-in-the-dark" "A Knife in the Dark"

-- | While Weathertop is in the victory display, each enemy in play gets -1 threat.
instance HasModifiersFor AKnifeInTheDark where
  getModifiersFor (AKnifeInTheDark a) =
    when (toCardCode Locations.weathertop `elem` map toCardCode a.victoryDisplay) do
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [ThreatModifier (-1)]

{- | A Knife in the Dark, Hunted, The Ring and The Nazgûl make up the encounter deck (The
Black Riders rules insert). Stage 1A sets aside all copies of Ringwraith, The Witch-king,
Midgewater and Weathertop, out of play: the scenario does it as it gathers.

The campaign card has no setup. Resolution: Starting with the first player, each player may
choose one of the following boon cards and attach it to a hero he controls: Tireless
Ranger, Skilled Healer, Valiant Warrior, Noble Hero. The chosen cards have been earned by
the players. Each is Permanent, so it is recorded on its hero, for its player's seat (The
Black Riders rules insert, "Permanent").
-}
instance RunMessage AKnifeInTheDark where
  runMessage msg (AKnifeInTheDark attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.AKnifeInTheDark, EncounterSet.Hunted, EncounterSet.TheRing, EncounterSet.TheNazgul]
      let aside = CardMatchAny $ map cardIs [Enemies.ringwraith, Enemies.theWitchKing, Locations.midgewater, Locations.weathertop]
          (outOfPlay, deck) = partition (cardMatches aside) cards
      putQuestIntoPlay =<< genEncounterCard Quests.troubleInBree
      laterStages <- traverse genEncounterCard [Quests.intoTheWild, Quests.theRingwraithsAttack]
      pure
        $ AKnifeInTheDark
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ outOfPlay
        & questDeckL .~ laterStages
    ResolveCampaign | isJust attrs.campaign -> do
      order <- getsGame (.playerOrder)
      eachPlayerMayTakeOneOf
        "aKnifeInTheDark.chooseBoon"
        Nothing
        [Attachments.tirelessRanger, Attachments.skilledHealer, Attachments.valiantWarrior, Attachments.nobleHero]
        \p boon -> do
          heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId p)
          chooseOrRunOneM p do
            questionLabeled "campaign.attachTo"
            targets heroes \h ->
              updateCampaignLog $ AddToPool $ PoolCard (toCardCode boon) (elemIndex p order) (Just $ toCardCode h)
      AKnifeInTheDark <$> runMessage msg attrs
    _ -> AKnifeInTheDark <$> runMessage msg attrs
