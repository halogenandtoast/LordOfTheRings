module LordOfTheRings.Enemy.Cards.WolvesFromMordor (wolvesFromMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WolvesFromMordor = WolvesFromMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wolvesFromMordor :: EnemyCard WolvesFromMordor
wolvesFromMordor = enemy WolvesFromMordor Cards.wolvesFromMordor

instance HasAbilities WolvesFromMordor where
  getAbilities (WolvesFromMordor a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage WolvesFromMordor where
  runMessage msg e@(WolvesFromMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      destroyed <- attackDestroyedACharacter attrs.id
      when destroyed $ shuffleIntoEncounterDeck attrs
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 2
      pure e
    _ -> WolvesFromMordor <$> runMessage msg attrs
