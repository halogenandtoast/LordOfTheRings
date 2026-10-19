module LordOfTheRings.Helpers.Character (module LordOfTheRings.Helpers.Character) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | The card definition behind a hero or ally target.
characterCardDef :: (HasCallStack, HasGame m) => Target -> m CardDef
characterCardDef = \case
  HeroTarget hid -> toCardDef <$> getHero hid
  AllyTarget aid -> toCardDef <$> getAlly aid
  target -> error $ "not a character: " <> show target

characterStat :: (HasGame m) => (CardDef -> Maybe Int) -> (ModifierType -> Maybe Int) -> Target -> m Int
characterStat printed mods target = do
  def <- characterCardDef target
  modifiedValue mods target (fromMaybe 0 $ printed def)

getWillpower, getAttack, getDefense, getHitPoints :: (HasGame m) => Target -> m Int
getWillpower = characterStat cdWillpower willpowerOf
getAttack = characterStat cdAttack attackOf
getDefense = characterStat cdDefense defenseOf
getHitPoints = characterStat cdHitPoints hitPointsOf
