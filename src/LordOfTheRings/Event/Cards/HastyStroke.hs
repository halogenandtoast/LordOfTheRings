module LordOfTheRings.Event.Cards.HastyStroke (hastyStroke) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype HastyStroke = HastyStroke EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Response: Cancel a shadow effect just triggered during combat.
hastyStroke :: EventCard HastyStroke
hastyStroke = eventWith HastyStroke Cards.hastyStroke (windowL ?~ ShadowEffectWindow When)

instance RunMessage HastyStroke where
  runMessage msg e@(HastyStroke attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ [cid | Window _ (ShadowEffectTriggered cid _) <- windows] \cid ->
        popMessagesMatching \case
          ResolveShadowCard cid' _ _ _ -> cid == cid'
          _ -> False
      pure e
    _ -> HastyStroke <$> runMessage msg attrs
