module LordOfTheRings.Enemy.Cards.OldTomnoddy (oldTomnoddy) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype OldTomnoddy = OldTomnoddy EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Venom, on its card def.
oldTomnoddy :: EnemyCard OldTomnoddy
oldTomnoddy = enemy OldTomnoddy Cards.oldTomnoddy

-- | Forced: At the end of each round, each player must give a hero he controls 1 poison.
instance HasAbilities OldTomnoddy where
  getAbilities (OldTomnoddy a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage OldTomnoddy where
  runMessage msg e@(OldTomnoddy attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        able <- filterM (fmap (notElem CannotBePoisoned) . getModifiers) heroes
        chooseOrRunOneM pid do
          questionLabeled "oldTomnoddy.poison"
          causedBy attrs
          targets able \h -> givePoison h 1
      pure e
    _ -> OldTomnoddy <$> runMessage msg attrs
