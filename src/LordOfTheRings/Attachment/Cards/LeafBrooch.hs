module LordOfTheRings.Attachment.Cards.LeafBrooch (leafBrooch) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (EventType), cdSphere, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gameEffects)

newtype LeafBrooch = LeafBrooch AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, on its card def.
leafBrooch :: AttachmentCard LeafBrooch
leafBrooch = attachment LeafBrooch Cards.leafBrooch & attachTo (AttachToHero AnyHero)

-- | The events of the attached hero's printed sphere.
matching :: HasGame m => AttachmentAttrs -> m (Maybe CardMatcher)
matching a = case a.placement of
  AttachedTo (HeroTarget hid) -> fmap (\s -> CardWithType EventType <> CardWithSphere s) . cdSphere . toCardDef <$> getHero hid
  _ -> pure Nothing

used :: AttachmentAttrs -> ModifierType
used _ = PlayedThisRound (toCardCode Cards.leafBrooch)

-- | The first event card you play each round that matches attached hero's printed sphere
-- gains secrecy 1.
-- Read as the cost secrecy 1 takes off while your threat is 20 or less, as the player's
-- costModifier takes it off a card with the keyword; nothing else reads secrecy.
instance HasModifiersFor LeafBrooch where
  getModifiersFor (LeafBrooch a) = do
    -- read from the effects, as the modifiers are being gathered
    spent <- getsGame (any (\e -> effectTarget e == toTarget a && used a `elem` effectModifiers e) . gameEffects)
    threat <- (.threat) <$> getPlayer a.controller
    unless (spent || threat > 20) $ matching a >>= traverse_ \m -> modified a (PlayerTarget a.controller) [ReduceCostOf m 1]

-- | Once such an event is played, the brooch is spent for the round.
instance HasAbilities LeafBrooch where
  getAbilities (LeafBrooch a) =
    [ restricted a 1 (TargetLacksModifier (toTarget a) (used a)) $ silent $ PlayedCardFromHandWindow After a.controller (CardWithType EventType)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage LeafBrooch where
  runMessage msg s@(LeafBrooch a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      m <- matching a
      when (or [cardMatches matcher code | Just matcher <- [m], Window _ (PlayedCardFromHand _ code) <- windows])
        $ roundModifier a a (used a)
      pure s
    _ -> LeafBrooch <$> runMessage msg a
