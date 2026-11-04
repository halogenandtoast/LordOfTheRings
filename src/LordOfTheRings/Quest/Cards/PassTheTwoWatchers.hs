module LordOfTheRings.Quest.Cards.PassTheTwoWatchers (passTheTwoWatchers) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMountainOfFire.Helpers (theOneRing)

newtype PassTheTwoWatchers = PassTheTwoWatchers QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

passTheTwoWatchers :: QuestCard PassTheTwoWatchers
passTheTwoWatchers = quest PassTheTwoWatchers Cards.passTheTwoWatchers

-- | Each non-unique location gets -1 threat and cannot leave the staging area. While The
-- Two Watchers is in the staging area (enemy side faceup), it is considered to be engaged
-- with each player.
instance HasModifiersFor PassTheTwoWatchers where
  getModifiersFor (PassTheTwoWatchers a) = when a.flipped do
    locations <- select (not_ UniqueLocation)
    for_ locations \l -> modified a l [ThreatModifier (-1), CannotLeaveStagingArea]
    watchers <- select (EnemyWithCardCode (toCardCode Enemies.theTwoWatchersEnemy) <> EnemyInStagingArea)
    for_ watchers \e -> modified a e [EngagedWithEachPlayer AnyPlayer]

-- | This stage cannot be defeated while The Two Watchers is in play: with no quest points, it
-- is defeated once the enemy side is.
instance HasAbilities PassTheTwoWatchers where
  getAbilities (PassTheTwoWatchers a) = [mkAbility a 1 $ silent $ EnemyDestroyedWindow After (cardIs Enemies.theTwoWatchersEnemy) | a.flipped]

{- | Setup: Set Shagrat aside, out of play (the scenario sets it aside as it gathers). Add The
Two Watchers (location side faceup) and Topmost Chamber to the staging area. The first
player loses control of the Ring-bearer, places it faceup under Topmost Chamber as a
guarded objective. Then, the first player attaches Last of the Company to a hero he
controls and attaches The One Ring to that hero.

The One Ring goes to the new hero before the Ring-bearer leaves, so it never leaves play.
The Ring-bearer under Topmost Chamber is out of play: its tokens go, and its attachments
are discarded, but for A Heavy Burden, which the campaign card attaches to the hero with
Last of the Company.
-}
instance RunMessage PassTheTwoWatchers where
  runMessage msg q@(PassTheTwoWatchers attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.theTwoWatchers)
      addSetAsideToStaging (cardIs Locations.topmostChamber)
      doStep 1 msg
      PassTheTwoWatchers <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      lead <- getLead
      ring <- theOneRing
      chamber <- select (LocationWithCardCode $ toCardCode Locations.topmostChamber)
      company <- getSetAside (cardIs Objectives.lastOfTheCompany)
      for_ ring \r -> for_ [hid | AttachedTo (HeroTarget hid) <- [r.placement]] \ringBearer -> do
        heroes <- filter (/= ringBearer) <$> select (HeroControlledBy $ PlayerWithId lead)
        chooseOrRunOneM lead do
          questionLabeled "passTheTwoWatchers.lastOfTheCompany"
          causedBy attrs
          targets heroes \hid -> do
            for_ company (`putObjectiveIntoPlayAttached` HeroTarget hid)
            placeObjective (toId r) (AttachedTo $ HeroTarget hid)
            attachments <- getAttachmentsOn (HeroTarget ringBearer)
            for_ [t | t <- attachments, t /= toTarget r, not (isObjective t)] discard
            hero <- getHero ringBearer
            when (hero.damage > 0) $ removeTokens attrs (HeroTarget ringBearer) Damage hero.damage
            when (hero.resources > 0) $ removeTokens attrs (HeroTarget ringBearer) Resource hero.resources
            for_ chamber \c -> guardCharacterWith (HeroTarget ringBearer) =<< getLocation c
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ CompleteQuest attrs.id
      pure q
    _ -> PassTheTwoWatchers <$> runMessage msg attrs
   where
    isObjective = \case
      ObjectiveTarget _ -> True
      _ -> False
