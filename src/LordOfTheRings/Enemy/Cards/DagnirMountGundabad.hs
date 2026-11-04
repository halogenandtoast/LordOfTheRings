module LordOfTheRings.Enemy.Cards.DagnirMountGundabad (dagnirMountGundabad) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype Dagnir = Dagnir EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dagnirMountGundabad :: EnemyCard Dagnir
dagnirMountGundabad = enemy Dagnir Cards.dagnirMountGundabad

{- | Indestructible. Immune to player card effects. X is 1 more than the number of quest
cards in the victory display. Its threat is X.
Cannot take more than X damage each round.
While there are 5 or more quest cards in the victory display, Dagnir cannot leave the
staging area and is considered to be engaged with each player.
-}
instance HasModifiersFor Dagnir where
  getModifiersFor (Dagnir a) = do
    quests <- questCardsInVictoryDisplay
    let x = quests + 1
    modifySelf a [ImmuneToPlayerCardEffects, ThreatModifier x]
    when (quests >= 5)
      $ modifySelf
        a
        [ CannotLeaveStagingArea
        , NoEngagementChecks
        , CannotBeOptionallyEngagedBy AnyPlayer
        , EngagedWithEachPlayer AnyPlayer
        ]

{- | Cannot take more than X damage each round: what she has taken this round is kept as a
round effect and X is read as the damage is placed, so a quest card added to the victory
display later in the round lets her take more. Dagnir's Wrath's 5 damage "is not limited by
Dagnir's text, and does not count toward her X value".
-}
instance RunMessage Dagnir where
  runMessage msg (Dagnir attrs) = case msg of
    PlaceTokens source (isTarget attrs -> True) Damage n | n > 0 -> do
      wrath <- case source of
        QuestSource qid -> (== Just (toCardCode Quests.dagnirsWrath)) . fmap toCardCode <$> getsGame (Map.lookup qid . (.quests) . gameEntities)
        _ -> pure False
      if wrath
        then pure $ Dagnir $ attrs & tokensL %~ addTokens Damage n
        else do
          x <- dagnirX
          Dagnir <$> placeDamageWithinRoundLimit x n attrs
    _ -> Dagnir <$> runMessage msg attrs
