module LordOfTheRings.Enemy.Cards.UngoliantsBrood (ungoliantsBrood) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype UngoliantsBrood = UngoliantsBrood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ungoliantsBrood :: EnemyCard UngoliantsBrood
ungoliantsBrood = enemy UngoliantsBrood Cards.ungoliantsBrood

instance HasAbilities UngoliantsBrood where
  getAbilities (UngoliantsBrood a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Forced: After you engage Ungoliant's Brood, your cards cannot ready for the remainder of
-- the round.
-- Shadow: If this attack is undefended, exhaust all characters you control.
instance RunMessage UngoliantsBrood where
  runMessage msg e@(UngoliantsBrood attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      -- ponytail: the characters in play now; one entering play later this round may ready
      for_ [p | EngagedWith p <- [attrs.placement]] \pid ->
        traverse_ (\c -> roundModifier attrs c CannotReady) =<< charactersControlledBy pid
      pure e
    ResolveShadowEffect cid _ pid defender | cid == attrs.cardId -> do
      when (null defender) $ traverse_ exhaust =<< charactersControlledBy pid
      pure e
    _ -> UngoliantsBrood <$> runMessage msg attrs
