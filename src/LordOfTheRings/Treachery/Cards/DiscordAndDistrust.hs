module LordOfTheRings.Treachery.Cards.DiscordAndDistrust (discordAndDistrust) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DiscordAndDistrust = DiscordAndDistrust TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

discordAndDistrust :: TreacheryCard DiscordAndDistrust
discordAndDistrust = treachery DiscordAndDistrust Cards.discordAndDistrust

instance RunMessage DiscordAndDistrust where
  runMessage msg t@(DiscordAndDistrust a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      roundModifier a GameTarget CannotTriggerActionsOrResponses
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders (\defender -> roundModifier a defender CannotReady)
      pure t
    _ -> DiscordAndDistrust <$> runMessage msg a
