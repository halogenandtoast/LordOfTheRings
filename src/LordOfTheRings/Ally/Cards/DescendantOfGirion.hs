module LordOfTheRings.Ally.Cards.DescendantOfGirion (descendantOfGirion) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Item))

newtype DescendantOfGirion = DescendantOfGirion AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

descendantOfGirion :: AllyCard DescendantOfGirion
descendantOfGirion = ally DescendantOfGirion Cards.descendantOfGirion

-- | Reduce the cost to play Item attachments on Descendant of Girion by 1.
instance HasModifiersFor DescendantOfGirion where
  getModifiersFor (DescendantOfGirion a) = modifySelf a [ReduceCostToAttach (CardWithTrait Item) 1]

instance RunMessage DescendantOfGirion where
  runMessage msg (DescendantOfGirion a) = DescendantOfGirion <$> runMessage msg a
