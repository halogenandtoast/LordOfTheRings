module LordOfTheRings.Attachment.Cards.FavorOfTheValar (favorOfTheValar) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype FavorOfTheValar = FavorOfTheValar AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a player's threat dial. Limit 1 per player, on its card def.
favorOfTheValar :: AttachmentCard FavorOfTheValar
favorOfTheValar = attachment FavorOfTheValar Cards.favorOfTheValar & attachTo AttachToPlayer

-- | Forced: When you would be eliminated by reaching your threat elimination level,
-- instead discard Favor of the Valar and reduce your threat to 5 less than your threat
-- elimination level. You are not eliminated.
instance HasAbilities FavorOfTheValar where
  getAbilities (FavorOfTheValar a) =
    [mkAbility a 1 $ forced $ EliminatedByThreatWindow When pid | AttachedTo (PlayerTarget pid) <- [a.placement]]

instance RunMessage FavorOfTheValar where
  runMessage msg s@(FavorOfTheValar a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \case
        PlayerTarget pid -> do
          cancelMatching (== EliminatePlayer pid)
          discard a a
          level <- getEliminationLevel pid
          threat <- (.threat) <$> getPlayer pid
          lowerThreatBy a pid (threat - (level - 5))
        _ -> pure ()
      pure s
    _ -> FavorOfTheValar <$> runMessage msg a
