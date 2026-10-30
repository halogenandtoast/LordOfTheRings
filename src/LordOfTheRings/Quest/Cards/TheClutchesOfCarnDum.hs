module LordOfTheRings.Quest.Cards.TheClutchesOfCarnDum (theClutchesOfCarnDum) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum.Helpers (flipThaurdir, getThaurdir, thaurdirCaptain, thaurdirChampion)

newtype TheClutchesOfCarnDum = TheClutchesOfCarnDum QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theClutchesOfCarnDum :: QuestCard TheClutchesOfCarnDum
theClutchesOfCarnDum = quest TheClutchesOfCarnDum Cards.theClutchesOfCarnDum

-- | Thaurdir cannot leave the staging area and cannot take damage.
-- Do not discard unresolved shadow cards at the end of the combat phase.
instance HasModifiersFor TheClutchesOfCarnDum where
  getModifiersFor (TheClutchesOfCarnDum a) = when a.flipped do
    modified a GameTarget [KeepUnresolvedShadowCards]
    getThaurdir >>= traverse_ \t -> modified a (EnemyTarget t) [CannotTakeDamage]

-- | At the end of the round, if Thaurdir is Champion side faceup, flip him.
instance HasAbilities TheClutchesOfCarnDum where
  getAbilities (TheClutchesOfCarnDum a) = [restricted a 1 (EnemyExists thaurdirChampion) $ forced RoundEndsWindow | a.flipped]

-- | Setup: Add Thaurdir to the staging area, Captain side faceup. Add 1 copy of Carn Dûm
-- Garrison per player to the staging area. Search the encounter deck for 1 copy of Accursed
-- Battlefield and make it the active location. Shuffle the encounter deck.
--
-- Forced: If there are 3 shadow cards dealt to Thaurdir, flip him. Checked as each is
-- dealt to him Captain side faceup: as Champion he is dealt one for the attack he makes as
-- he flips, which resolves them all.
instance RunMessage TheClutchesOfCarnDum where
  runMessage msg q@(TheClutchesOfCarnDum attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getRemainingPlayers
      let copies def = filter ((== toCardCode def) . toCardCode) deck
          taking n def f = for_ (take n $ copies def) \card -> do
            removeFromEncounterDeck card
            f card
      taking 1 Enemies.thaurdirCaptain addToStaging
      taking players Enemies.carnDumGarrison addToStaging
      taking 1 Locations.accursedBattlefield putIntoPlayAsActiveLocation
      shuffleEncounterDeck
      pure $ TheClutchesOfCarnDum $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      flipThaurdir
      pure q
    -- the shadow card is counted once it is dealt
    AttachShadowCard eid _ | attrs.flipped -> do
      captain <- selectAny (EnemyWithId eid <> thaurdirCaptain)
      when captain $ checkStage attrs.id
      TheClutchesOfCarnDum <$> runMessage msg attrs
    CheckStage qid | qid == attrs.id -> do
      captain <- select thaurdirCaptain
      for_ captain \t -> do
        dealt <- length . (.shadowCards) <$> getEnemy t
        when (dealt >= 3) flipThaurdir
      pure q
    _ -> TheClutchesOfCarnDum <$> runMessage msg attrs
