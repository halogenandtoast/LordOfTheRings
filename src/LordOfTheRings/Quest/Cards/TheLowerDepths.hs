module LordOfTheRings.Quest.Cards.TheLowerDepths (theLowerDepths) where

import LordOfTheRings.Card (cdEncounterSet, toCardCode, toCardDef)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheLowerDepths = TheLowerDepths QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theLowerDepths :: QuestCard TheLowerDepths
theLowerDepths = quest TheLowerDepths Cards.theLowerDepths

-- | Fire-drake cannot leave the staging area or take damage.
instance HasModifiersFor TheLowerDepths where
  getModifiersFor (TheLowerDepths a) = when a.flipped do
    drakes <- select (EnemyWithCardCode $ toCardCode Enemies.fireDrake)
    for_ drakes \d -> modified a d [CannotLeaveStagingArea, CannotTakeDamage]

{- | Response: After placing progress here from questing successfully, remove 5 progress
from this stage to look at the top 2 cards of the Caves deck. Put one on top of the Caves
deck and put the other on the bottom. Progress was placed here from questing if this
stage took progress this quest phase; with fewer than 5 on it the cost cannot be paid.
Forced: After Dragon Hoard becomes the active location, advance to stage 3A.
-}
instance HasAbilities TheLowerDepths where
  getAbilities (TheLowerDepths a) =
    [ restricted a 1 (ProgressPlacedThisPhaseOn $ toTarget a) $ response $ PlacedProgressFromQuestingWindow After
    | a.flipped
    , a.progress >= 5
    ]
      <> [ mkAbility a 2 $ forced $ LocationBecameActiveWindow After (LocationWithCardCode $ toCardCode Locations.dragonHoard)
         | a.flipped
         ]

-- | When Revealed: Place Dragon Hoard on the bottom of the Caves deck. Add Fire-drake to
-- the staging area and shuffle the Dragon Might encounter set into the encounter deck.
instance RunMessage TheLowerDepths where
  runMessage msg q@(TheLowerDepths attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheLowerDepths <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ putOnBottomOfCavesDeck =<< getSetAside (cardIs Locations.dragonHoard)
      addSetAsideToStaging (cardIs Enemies.fireDrake)
      shuffleSetAsideIntoEncounterDeck . filter ((== Just EncounterSet.DragonMight) . cdEncounterSet . toCardDef) =<< getSetAside AnyCard
      pure q
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      removeTokens attrs attrs Progress 5
      arrangeTopOfCavesDeck pid attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      advanceQuest
      pure q
    _ -> TheLowerDepths <$> runMessage msg attrs
