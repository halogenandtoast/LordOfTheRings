module LordOfTheRings.Location.Cards.WesternEmynMuil (westernEmynMuil) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Engagement (highestToughnessEngagements)
import LordOfTheRings.Keyword (Keyword (Toughness))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea), placementController)
import LordOfTheRings.Trait (Trait (UrukHai))

newtype WesternEmynMuil = WesternEmynMuil LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

westernEmynMuil :: LocationCard WesternEmynMuil
westernEmynMuil = location WesternEmynMuil Cards.westernEmynMuil

instance HasModifiersFor WesternEmynMuil where
  getModifiersFor (WesternEmynMuil a) = do
    modifySelf a [TravelRequiresEngagingHighestToughness]
    when (a.placement == StagingArea) do
      enemies <- select (EnemyWithTrait UrukHai)
      players <- getStagePlayers
      for_ enemies \eid -> do
        enemy <- getEnemy eid
        when (maybe True (`elem` players) $ placementController enemy.placement) $
          modified a eid [GainKeyword $ Toughness 1]

instance RunMessage WesternEmynMuil where
  runMessage msg l@(WesternEmynMuil a) = case msg of
    TravelTo lid | lid == a.id -> do
      whenM (notElem Blank <$> getModifiers a) do
        lead <- getStageLead
        candidates <- highestToughnessEngagements
        chooseOrRunOneM lead do
          questionLabeled "westernEmynMuil.player"
          causedBy a
          targets (ordNub $ map fst candidates) \pid -> chooseOrRunOneM pid do
            questionLabeled "westernEmynMuil.enemy"
            causedBy a
            targets [eid | (p, eid) <- candidates, p == pid] (engageEnemy pid)
      pure l
    _ -> WesternEmynMuil <$> runMessage msg a
