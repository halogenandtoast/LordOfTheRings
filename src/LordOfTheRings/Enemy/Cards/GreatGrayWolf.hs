module LordOfTheRings.Enemy.Cards.GreatGrayWolf (greatGrayWolf) where

import LordOfTheRings.Card (cdRiddle, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Creature))

newtype GreatGrayWolf = GreatGrayWolf EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatGrayWolf :: EnemyCard GreatGrayWolf
greatGrayWolf = enemy GreatGrayWolf Cards.greatGrayWolf

{- | Forced: If Great Gray Wolf is dealt a shadow card with a riddle, return all Creature
enemies to the staging area at the end of the combat phase. Read as its shadow cards turn
up, as Warg-rider's: the Creature enemies in play at the end of this round's combat phase
return then, the Wolf among them or not.
-}
instance RunMessage GreatGrayWolf where
  runMessage msg (GreatGrayWolf attrs) = case msg of
    ResolveShadow eid _ _ | eid == attrs.id -> do
      let faceDown = filter ((`notElem` attrs.enemyShadowRevealed) . (.id)) attrs.shadowCards
      when (any (isJust . cdRiddle . toCardDef) faceDown)
        $ roundModifier attrs GameTarget (ReturnToStagingAtEndOfCombat $ EnemyWithTrait Creature)
      GreatGrayWolf <$> runMessage msg attrs
    _ -> GreatGrayWolf <$> runMessage msg attrs
