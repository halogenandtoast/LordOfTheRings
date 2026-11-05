module LordOfTheRings.Enemy.Cards.StoneGiant (stoneGiant) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype StoneGiant = StoneGiant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | While at least one Stone-giant is in the staging area, the Galloping Boulders card
gains surge: Galloping Boulders asks as it is revealed.
-}
stoneGiant :: EnemyCard StoneGiant
stoneGiant = enemy StoneGiant Cards.stoneGiant

{- | Forced: After Stone-giant engages a player, that player chooses and discards 1 ally
he controls. (1)

The Stone-giant Guffawing of Giants chose engages the player with the highest threat at
the end of the quest phase, the first player choosing among those tied. (2)
-}
instance HasAbilities StoneGiant where
  getAbilities (StoneGiant a) =
    [ mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id
    , restricted a 2 (TargetHasModifier (toTarget a) EngagesHighestThreatAtEndOfQuestPhase)
        $ forced
        $ PhaseEndsWindow When QuestPhase
    ]

instance RunMessage StoneGiant where
  runMessage msg e@(StoneGiant attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged eid pid) <- ws, eid == attrs.id] \pid -> do
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "stoneGiant.discardAlly"
          causedBy attrs
          targets allies (discard attrs)
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      withHighestThreatPlayer attrs \pid -> engageEnemy pid attrs.id
      pure e
    _ -> StoneGiant <$> runMessage msg attrs
