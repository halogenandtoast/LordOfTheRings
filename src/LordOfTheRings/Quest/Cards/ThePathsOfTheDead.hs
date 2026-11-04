module LordOfTheRings.Quest.Cards.ThePathsOfTheDead (thePathsOfTheDead) where

import LordOfTheRings.Card (CardType (LocationType), cdName, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ThePathsOfTheDead = ThePathsOfTheDead QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

thePathsOfTheDead :: QuestCard ThePathsOfTheDead
thePathsOfTheDead = quest ThePathsOfTheDead Cards.thePathsOfTheDead

-- | X is the threat of the player with the highest threat.
pathsX :: HasGame m => m Int
pathsX = foldl' max 0 . map (.threat) <$> (traverse getPlayer =<< getRemainingPlayers)

-- | X quest points. Each player cannot reduce his threat below his initial threat level.
instance HasModifiersFor ThePathsOfTheDead where
  getModifiersFor (ThePathsOfTheDead a) = when a.flipped do
    x <- pathsX
    modifySelf a [QuestPointModifier x]
    players <- traverse getPlayer =<< getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p.id) [CannotReduceThreatBelow p.startingThreat]

-- | Forced: At the beginning of the planning phase each player must choose: either raise
-- your threat by 1, or you cannot play cards this phase.
instance HasAbilities ThePathsOfTheDead where
  getAbilities (ThePathsOfTheDead a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After PlanningPhase | a.flipped]

{- | When Revealed: Each player searches the encounter deck for a different location and
adds it to the staging area. Shuffle the encounter deck. (One after another in turn order,
each passing over the titles the players before him took; the staging area is empty as
stage 1B advances here at the end of the first planning phase.)

The stage is defeated once its progress reaches X, which moves as the players' threat
does.
-}
instance RunMessage ThePathsOfTheDead where
  runMessage msg q@(ThePathsOfTheDead attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      ThePathsOfTheDead <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure q
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (atMay players (n - 1)) \p -> do
        staged <- traverse getLocation =<< select LocationInStagingArea
        let taken = [t | l <- staged, let Name t _ = cdName (toCardDef l)]
        searchEncounterDeckToStaging p (CardMatchAll $ CardWithType LocationType : map (NotCard . CardWithTitle) taken)
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> chooseOneM p do
        questionLabeled "thePathsOfTheDead.choose"
        causedBy attrs
        labeled "thePathsOfTheDead.raiseThreat" $ encounterRaisesThreat p 1
        labeled "thePathsOfTheDead.cannotPlay" $ phaseModifier attrs (PlayerTarget p) (CannotPlay AnyCard)
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> checkX =<< runMessage msg attrs
    CheckStage qid | qid == attrs.id -> checkX attrs
    -- a lower threat lowers X
    LowerThreat {} -> do
      push $ CheckStage attrs.id
      pure q
    _ -> ThePathsOfTheDead <$> runMessage msg attrs
   where
    checkX a = do
      x <- pathsX
      when (a.flipped && x > 0 && a.progress >= x) $ push (CompleteQuest a.id)
      pure $ ThePathsOfTheDead a
