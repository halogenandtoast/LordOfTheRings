module LordOfTheRings.Attachment.Cards.LegacyOfDurin (legacyOfDurin) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype LegacyOfDurin = LegacyOfDurin AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf hero.
legacyOfDurin :: AttachmentCard LegacyOfDurin
legacyOfDurin = attachment LegacyOfDurin Cards.legacyOfDurin & attachTo (AttachToHero $ HeroWithTrait Dwarf)

-- | Response: After you play a Dwarf character from your hand, exhaust Legacy of Durin to
-- draw 1 card.
instance HasAbilities LegacyOfDurin where
  getAbilities (LegacyOfDurin a) =
    [ mkAbility a 1
        $ ResponseAbility (PlayedCardFromHandWindow After a.owner $ CardWithTrait Dwarf <> CardWithType AllyType) (ExhaustCost $ toTarget a)
    ]

instance RunMessage LegacyOfDurin where
  runMessage msg s@(LegacyOfDurin a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards pid 1
      pure s
    _ -> LegacyOfDurin <$> runMessage msg a
