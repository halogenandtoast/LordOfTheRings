module LordOfTheRings.Ally.Cards.Eldahir (eldahir) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyShadowRevealed))
import LordOfTheRings.Sphere (Sphere (Lore))

newtype Eldahir = Eldahir AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on his card def.
eldahir :: AllyCard Eldahir
eldahir = ally Eldahir Cards.eldahir

watched :: EnemyMatcher
watched = EnemyEngagedWith AnyPlayer <> EnemyWithFacedownShadow

-- | Action: Spend 1 lore resource to look at a facedown shadow card dealt to an engaged
-- enemy. If that card has a shadow effect, Eldahir gets +2 defense until the end of the
-- phase. Any player may trigger this action. (Limit once per phase.)
instance HasAbilities Eldahir where
  getAbilities (Eldahir a) =
    [ anyPlayer
        $ limited (GroupLimit PerPhase 1)
        $ restricted a 1 (exists watched)
        $ ActionAbility (SpendResourcesCost Lore 1)
    ]

instance RunMessage Eldahir where
  runMessage msg s@(Eldahir a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select watched
      chooseOrRunOneM pid do
        questionLabeled "eldahir.enemy"
        causedBy a
        targets enemies \eid -> do
          enemy <- getEnemy eid
          shadows <- shadowsPlayerCardsCanAffect enemy
          let facedown = [c | c <- shadows, c.id `notElem` enemyShadowRevealed (toAttrs enemy)]
          chooseOrRunOneM pid do
            questionLabeled "eldahir.shadow"
            causedBy a
            targets facedown \card -> do
              lookAtShadowCard pid a eid card.id
              when (cdShadowEffect $ toCardDef card) $ phaseModifier a a (DefenseModifier 2)
      pure s
    _ -> Eldahir <$> runMessage msg a
