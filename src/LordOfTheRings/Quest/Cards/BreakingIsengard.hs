module LordOfTheRings.Quest.Cards.BreakingIsengard (breakingIsengard) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype BreakingIsengard = BreakingIsengard QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

breakingIsengard :: QuestCard BreakingIsengard
breakingIsengard = quest BreakingIsengard Cards.breakingIsengard

-- | The players cannot travel to Orthanc.
instance HasModifiersFor BreakingIsengard where
  getModifiersFor (BreakingIsengard a) = when a.flipped do
    orthancs <- select (LocationWithCardCode $ toCardCode Locations.orthanc)
    for_ orthancs \l -> modified a l [CannotBeTraveledTo]

{- | Forced: After a non-unique location is explored, place 1 damage here. Then, if there
are 5 damage tokens here, advance to stage 3A.
Quest Action: Exhaust Ent of Fangorn to place 2 progress on a non-unique location.
-}
instance HasAbilities BreakingIsengard where
  getAbilities (BreakingIsengard a)
    | a.flipped =
        [ mkAbility a 1 $ forced $ ExploredMatchingWindow After (CardWithType LocationType <> NotCard UniqueCard)
        , anyPlayer $ restricted a 2 (DuringPhase QuestPhase <> exists nonUnique)
            $ ActionAbility (ExhaustCharacterCost (NotHero AnyHero) (AllyWithCardCode (toCardCode Objectives.entOfFangorn) <> AllyControlledBy ActivePlayer))
        ]
    | otherwise = []
   where
    nonUnique = NotLocation UniqueLocation

{- | When Revealed: Make Gates of Isengard the active location. Add Saruman and Orthanc to
the staging area. (Stage 1A set the three aside.)
-}
instance RunMessage BreakingIsengard where
  runMessage msg q@(BreakingIsengard attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      BreakingIsengard <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      gates <- filter (cardMatches $ cardIs Locations.gatesOfIsengard) <$> getSetAside AnyCard
      traverse_ putIntoPlayAsActiveLocation gates
      addSetAsideToStaging (cardIs Enemies.sarumanTheRoadToIsengard)
      addSetAsideToStaging (cardIs Locations.orthanc)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeTokens attrs attrs Damage 1
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      when (countTokens Damage attrs.questTokens >= 5) advanceQuest
      pure q
    UseCardAbility pid (isSource attrs -> True) 2 _ -> do
      locations <- select (NotLocation UniqueLocation)
      chooseOrRunOneM pid do
        questionLabeled "breakingIsengard.progress"
        causedBy attrs
        targets locations \l -> placeProgress attrs l 2
      pure q
    _ -> BreakingIsengard <$> runMessage msg attrs
