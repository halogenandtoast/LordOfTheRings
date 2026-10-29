module LordOfTheRings.Enemy.Cards.MugashsGuard (mugashsGuard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (engageCaptor, getMugash, mugashCaptor, mugashHost)

newtype MugashsGuard = MugashsGuard EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

mugashsGuard :: EnemyCard MugashsGuard
mugashsGuard = enemy MugashsGuard Cards.mugashsGuard

-- | Forced: After Mugash's Guard destroys a character, if Mugash is attached to a hero,
-- return Mugash to the staging area.
instance HasAbilities MugashsGuard where
  getAbilities (MugashsGuard a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

{- | While a player controls a hero with Mugash attached, Mugash's Guard engages that
player: as it enters play, and as Mugash is attached (see Mugash), checked as each window
opens, as anything moving it opens one; whatever would have another player engage it has
that player engage it.
-}
instance HasModifiersFor MugashsGuard where
  getModifiersFor (MugashsGuard a) = mugashCaptor >>= traverse_ \p -> modified a a [EngagesOnly p]

instance RunMessage MugashsGuard where
  runMessage msg e@(MugashsGuard attrs) = case msg of
    CheckWindows _ -> do
      engageCaptor attrs.id
      MugashsGuard <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      captured <- isJust <$> mugashHost
      when captured $ getMugash >>= traverse_ \m -> placeEnemy m.id StagingArea
      pure e
    _ -> MugashsGuard <$> runMessage msg attrs
