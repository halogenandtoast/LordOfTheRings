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
    modifySelf a [ImmuneToPlayerCardEffects, ThreatModifier x, DamageLimit x]
    when (quests >= 5)
      $ modifySelf
        a
        [ CannotLeaveStagingArea
        , NoEngagementChecks
        , CannotBeOptionallyEngagedBy AnyPlayer
        , EngagedWithEachPlayer AnyPlayer
        ]

-- | Dagnir's Wrath's 5 damage is not limited by her text, nor counted toward it.
instance RunMessage Dagnir where
  runMessage msg (Dagnir attrs) = case msg of
    PlaceTokens (QuestSource qid) (isTarget attrs -> True) Damage n -> do
      quest <- getsGame (Map.lookup qid . (.quests) . gameEntities)
      if fmap toCardCode quest == Just (toCardCode Quests.dagnirsWrath)
        then pure $ Dagnir $ attrs & tokensL %~ addTokens Damage n
        else Dagnir <$> runMessage msg attrs
    _ -> Dagnir <$> runMessage msg attrs
