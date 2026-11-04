module LordOfTheRings.Ally.Cards.DaleMessenger (daleMessenger) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DaleMessenger = DaleMessenger AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

daleMessenger :: AllyCard DaleMessenger
daleMessenger = ally DaleMessenger Cards.daleMessenger

-- | Response: After Dale Messenger enters play, choose a player. That player heals 1 damage
-- from each character he controls with a player card attachment.
instance HasAbilities DaleMessenger where
  getAbilities (DaleMessenger a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage DaleMessenger where
  runMessage msg s@(DaleMessenger a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      choosePlayerM pid a \p -> do
        let you = PlayerWithId p
        characters <- selectCharacters (HeroControlledBy you <> HeroWithPlayerAttachment) (AllyControlledBy you <> AllyWithPlayerAttachment)
        for_ characters \c -> healDamage a c 1
      pure s
    _ -> DaleMessenger <$> runMessage msg a
