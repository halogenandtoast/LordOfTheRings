module LordOfTheRings.Attachment.Cards.KahlielsHeaddress (kahlielsHeaddress) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Trait (Trait (Harad))

newtype KahlielsHeaddress = KahlielsHeaddress AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Kahliel. Restricted, on its card def.
kahlielsHeaddress :: AttachmentCard KahlielsHeaddress
kahlielsHeaddress = attachment KahlielsHeaddress Cards.kahlielsHeaddress & attachTo (AttachToHero $ HeroWithTitle "Kahliel")

haradAlly :: CardMatcher
haradAlly = CardWithType AllyType <> CardWithTrait Harad

-- | Each Harad character gets +1 willpower.
instance HasModifiersFor KahlielsHeaddress where
  getModifiersFor (KahlielsHeaddress a) = case a.placement of
    AttachedTo _ -> do
      harad <- selectCharacters (HeroWithTrait Harad) (AllyWithTrait Harad)
      for_ harad \c -> modified a c [WillpowerModifier 1]
    _ -> pure ()

-- | Refresh Action: Exhaust Kahliel's Headdress to shuffle the topmost Harad ally in your
-- discard pile into your deck.
instance HasAbilities KahlielsHeaddress where
  getAbilities (KahlielsHeaddress a) =
    [ restricted a 1 (DuringPhase RefreshPhase <> CardInDiscard a.controller haradAlly) $ ActionAbility (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage KahlielsHeaddress where
  runMessage msg s@(KahlielsHeaddress a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      -- the discard pile's top card comes first
      discards <- (.discard) <$> getPlayer pid
      for_ (find (cardMatches haradAlly) discards) \c -> shuffleFromDiscardIntoDeck pid c.id
      pure s
    _ -> KahlielsHeaddress <$> runMessage msg a
