module LordOfTheRings.Location.Cards.AbandonedCampNightmare (abandonedCampNightmare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype AbandonedCampNightmare = AbandonedCampNightmare LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

abandonedCampNightmare :: LocationCard AbandonedCampNightmare
abandonedCampNightmare = location AbandonedCampNightmare Cards.abandonedCampNightmare

-- | Immune to player card effects.
instance HasModifiersFor AbandonedCampNightmare where
  getModifiersFor (AbandonedCampNightmare a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After Abandoned Camp leaves play, each player must deal 2 damage to each
-- exhausted character he controls.
instance HasAbilities AbandonedCampNightmare where
  -- ponytail: as it leaves play explored, the way a location nearly always leaves
  getAbilities (AbandonedCampNightmare a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Shadow: If this attack is undefended, discard all attachment cards players control.
instance RunMessage AbandonedCampNightmare where
  runMessage msg l@(AbandonedCampNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage a c 2
      pure l
    ResolveShadowEffect cid _ _ defender | cid == a.cardId -> do
      when (null defender) do
        players <- getRemainingPlayers
        traverse_ discard . concat =<< traverse getAttachmentsControlledBy players
      pure l
    _ -> AbandonedCampNightmare <$> runMessage msg a
