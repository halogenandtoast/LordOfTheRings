module LordOfTheRings.Attachment.Cards.LordOfMorthond (lordOfMorthond) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (Trait (Gondor, Outlands))

newtype LordOfMorthond = LordOfMorthond AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Gondor or Outlands hero.
lordOfMorthond :: AttachmentCard LordOfMorthond
lordOfMorthond =
  attachment LordOfMorthond Cards.lordOfMorthond
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Gondor, HeroWithTrait Outlands])

-- | If each hero you control has a printed leadership resource icon, Lord of Morthond
-- gains: "Response: After you play a spirit, tactics or lore ally, draw 1 card."
instance HasAbilities LordOfMorthond where
  getAbilities (LordOfMorthond a) =
    [ restricted a 1 (eachHeroHasPrintedIcon a.controller Leadership)
        $ response
        $ PlayedCardFromHandWindow After a.controller
        $ CardWithType AllyType
        <> CardMatchAny [CardWithSphere Spirit, CardWithSphere Tactics, CardWithSphere Lore]
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage LordOfMorthond where
  runMessage msg s@(LordOfMorthond a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards pid 1
      pure s
    _ -> LordOfMorthond <$> runMessage msg a
