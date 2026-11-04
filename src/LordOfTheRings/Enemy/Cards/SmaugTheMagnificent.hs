module LordOfTheRings.Enemy.Cards.SmaugTheMagnificent (smaugTheMagnificent) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (hasBurgle)

newtype SmaugTheMagnificent = SmaugTheMagnificent EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

smaugTheMagnificent :: EnemyCard SmaugTheMagnificent
smaugTheMagnificent = enemy SmaugTheMagnificent Cards.smaugTheMagnificent

{- | Indestructible. Immune to player card effects.
Cannot leave the staging area (except by quest effect) but is considered to be engaged
with the first player. A quest effect sets him aside, which nothing else does.
-}
instance HasModifiersFor SmaugTheMagnificent where
  getModifiersFor (SmaugTheMagnificent a) =
    modifySelf
      a
      [ ImmuneToPlayerCardEffects
      , CannotLeaveStagingArea
      , NoEngagementChecks
      , CannotBeOptionallyEngagedBy AnyPlayer
      , EngagedWithEachPlayer FirstPlayer
      ]

{- | Forced: If Smaug the Magnificent is dealt a shadow card with a burgle effect, he makes
an additional attack immediately after this one. A shadow card is known to have a burgle
effect as it turns face up during his attack, as Lord Alcaron's "no shadow effect" is; he
attacks the first player, the only one he is engaged with.
-}
instance RunMessage SmaugTheMagnificent where
  runMessage msg e@(SmaugTheMagnificent attrs) = case msg of
    PlaceEnemy eid placement | eid == attrs.id && placement /= StagingArea -> pure e
    RevealShadowCard eid cid
      | eid == attrs.id
      , Just card <- find ((== cid) . (.id)) attrs.shadowCards
      , cid `notElem` attrs.enemyShadowRevealed
      , hasBurgle card -> do
          lead <- getLead
          additionalAttack lead attrs.id
          SmaugTheMagnificent <$> runMessage msg attrs
    _ -> SmaugTheMagnificent <$> runMessage msg attrs
