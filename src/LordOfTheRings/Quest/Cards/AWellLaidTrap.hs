module LordOfTheRings.Quest.Cards.AWellLaidTrap (aWellLaidTrap) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDunlandTrap.Helpers (eachPlayerSearchesToEngage)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Trait (Trait (BoarClan, Item, Mount))

newtype AWellLaidTrap = AWellLaidTrap QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aWellLaidTrap :: QuestCard AWellLaidTrap
aWellLaidTrap = quest AWellLaidTrap Cards.aWellLaidTrap

-- | Forced: At the end of the combat phase, advance to stage 3A.
instance HasAbilities AWellLaidTrap where
  getAbilities (AWellLaidTrap a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase]

-- | When Revealed: Make Munuv Dûv Ravine the active location (returning any previous
-- active location to the staging area). Each player discards each Item and each Mount
-- attachment he controls. Each player chooses 1 ally he controls and discards each other
-- ally he controls.
-- When Revealed: Each player searches the encounter deck and discard pile for a Boar Clan
-- enemy and puts it into play engaged with him. Shuffle the encounter deck. Each player
-- shuffles his discard pile into his draw deck and draws 1 card.
instance RunMessage AWellLaidTrap where
  runMessage msg q@(AWellLaidTrap attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      AWellLaidTrap <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (`placeLocation` StagingArea) =<< select IsActiveLocation
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ putIntoPlayAsActiveLocation $ filter (cardMatches $ cardIs Locations.munuvDuvRavine) aside
      attachments <- getsGame (toList . (.entities.attachments))
      for_ (filter (\x -> any (`member` cdCardTraits (toCardDef x)) [Item, Mount]) attachments) (discard attrs)
      eachPlayer \p -> do
        allies <- select (AllyControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "aWellLaidTrap.keepAlly"
          causedBy attrs
          targets allies \kept -> for_ (filter (/= kept) allies) (discard attrs)
      eachPlayerSearchesToEngage attrs BoarClan
      eachPlayer \p -> do
        shuffleDiscardIntoDeck p
        drawCards attrs p 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      completeQuest attrs.id
      pure q
    _ -> AWellLaidTrap <$> runMessage msg attrs
