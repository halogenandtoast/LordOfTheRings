module LordOfTheRings.Location.Cards.PlunderedArmoury (plunderedArmoury) where

import LordOfTheRings.Card (CardType (AttachmentType), toCardCode, toCardDef)
import LordOfTheRings.Card.CardDef (CardDef (..))
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Armor, Weapon))

newtype PlunderedArmoury = PlunderedArmoury LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

plunderedArmoury :: LocationCard PlunderedArmoury
plunderedArmoury = location PlunderedArmoury Cards.plunderedArmoury

-- | While Plundered Armoury is in the staging area, enemies get +1 attack.
instance HasModifiersFor PlunderedArmoury where
  getModifiersFor (PlunderedArmoury a) = when (a.placement == StagingArea) do
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [AttackModifier 1]

-- | Response: After Plundered Armoury leaves play as an explored location, each player may
-- attach a Weapon or Armour attachment from his hand to 1 character he controls.
instance HasAbilities PlunderedArmoury where
  getAbilities (PlunderedArmoury a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

instance RunMessage PlunderedArmoury where
  runMessage msg l@(PlunderedArmoury a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        player <- getPlayer pid
        mine <- charactersControlledBy pid
        let armoury c = c.cardType == AttachmentType && any (`member` cdCardTraits (toCardDef c)) [Weapon, Armor]
            cards = filter armoury player.hand
        unless (null cards) $ chooseOneM pid do
          questionLabeled "plunderedArmoury.attach"
          causedBy a
          targets cards \c -> do
            hosts <- filter (`elem` mine) <$> cardHosts pid (toCardCode c)
            chooseOneM pid do
              questionLabeled "plunderedArmoury.host"
              causedBy a
              targets hosts (attachFromHand pid c)
          labeled "pass" $ pure ()
      pure l
    _ -> PlunderedArmoury <$> runMessage msg a
