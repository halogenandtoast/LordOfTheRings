module LordOfTheRings.Attachment.Cards.TheRedArrow (theRedArrow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Gondor))

newtype TheRedArrow = TheRedArrow AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRedArrow :: AttachmentCard TheRedArrow
theRedArrow = attachment TheRedArrow Cards.theRedArrow & attachTo (AttachToHero $ HeroWithTrait Gondor)

-- | Attached hero gets +1 willpower.
instance HasModifiersFor TheRedArrow where
  getModifiersFor (TheRedArrow a) = withHost a \host -> modified a host [WillpowerModifier 1]

-- | Valour Response: After attached hero quests successfully, add The Red Arrow to the
-- victory display to search the top 5 cards of your deck for an ally and put it into play.
instance HasAbilities TheRedArrow where
  getAbilities (TheRedArrow a) =
    [ restricted a 1 (exists (HeroWithId hid <> CommittedHero) <> PlayerExists (PlayerWithId a.owner <> PlayerWithThreatAtLeast 40))
        $ response $ QuestedSuccessfullyWindow After
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage TheRedArrow where
  runMessage msg s@(TheRedArrow a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      -- ponytail: the victory display holds only encounter cards, so it leaves the game
      -- instead; it goes there once the display can hold player cards
      discard a
      removeFromDiscard a.owner a.cardId
      searchDeckIntoPlay pid a 5 (CardWithType AllyType) 1 []
      pure s
    _ -> TheRedArrow <$> runMessage msg a
