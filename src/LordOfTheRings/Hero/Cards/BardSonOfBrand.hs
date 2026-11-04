module LordOfTheRings.Hero.Cards.BardSonOfBrand (bardSonOfBrand) where

import LordOfTheRings.Card (CardType (AttachmentType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (Trait (Item))

newtype BardSonOfBrand = BardSonOfBrand HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

bardSonOfBrand :: HeroCard BardSonOfBrand
bardSonOfBrand = hero BardSonOfBrand Cards.bardSonOfBrand

-- | While playing an Item attachment, Bard son of Brand is considered to have the printed
-- leadership, lore, and tactics icons: his resources pay for it, and give a 0-cost one its
-- resource match.
instance HasModifiersFor BardSonOfBrand where
  getModifiersFor (BardSonOfBrand a) =
    modifySelf a [PrintedResourceIconFor (CardWithType AttachmentType <> CardWithTrait Item) s | s <- [Leadership, Lore, Tactics]]

-- | Response: When a character leaves play, return each attachment attached to that
-- character to its owner's hand. (Limit once per phase.)
instance HasAbilities BardSonOfBrand where
  getAbilities (BardSonOfBrand a) =
    [limited (PlayerLimit PerPhase 1) $ mkAbility a 1 $ response $ CharacterWithAttachmentsLeftPlayWindow When]

instance RunMessage BardSonOfBrand where
  runMessage msg h@(BardSonOfBrand attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ (take 1 [c | Window When (LeftPlay c _ _) <- windows]) \c ->
        traverse_ returnToHand =<< playerAttachmentsOn c
      pure h
    _ -> BardSonOfBrand <$> runMessage msg attrs
