module LordOfTheRings.Enemy.Cards.NazgulOfDolGuldur (nazgulOfDolGuldur) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype NazgulOfDolGuldur = NazgulOfDolGuldur EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

nazgulOfDolGuldur :: EnemyCard NazgulOfDolGuldur
nazgulOfDolGuldur = enemy NazgulOfDolGuldur Cards.nazgulOfDolGuldur

-- | No attachments can be played on Nazgul of Dol Guldur.
instance HasModifiersFor NazgulOfDolGuldur where
  getModifiersFor (NazgulOfDolGuldur a) = modified a a [CannotHaveAttachments]

-- | Forced: After a shadow effect dealt to Nazgul of Dol Guldur resolves, the engaged player
-- must choose and discard 1 character he controls.
-- (Moving into the staging area when the prisoner is rescued is done by the scenario,
-- since the Nazgul is out of play until then.)
instance HasAbilities NazgulOfDolGuldur where
  getAbilities (NazgulOfDolGuldur a) = [mkAbility a 1 $ forced $ ShadowEffectResolvedWindow After a.id]

instance RunMessage NazgulOfDolGuldur where
  runMessage msg e@(NazgulOfDolGuldur attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (placementController attrs.placement) \pid -> do
        characters <- charactersControlledBy pid
        push $ ChooseAndDiscard (toSource attrs) pid characters
      pure e
    _ -> NazgulOfDolGuldur <$> runMessage msg attrs
