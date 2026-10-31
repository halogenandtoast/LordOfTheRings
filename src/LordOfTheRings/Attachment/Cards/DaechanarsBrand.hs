module LordOfTheRings.Attachment.Cards.DaechanarsBrand (daechanarsBrand) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (EncounterCard), CardType (TreacheryType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Trait (Trait (Sorcery, Undead))

newtype DaechanarsBrand = DaechanarsBrand AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from The Treachery of Rhudaur's Clue. Attach to
-- a hero. Restricted.
daechanarsBrand :: AttachmentCard DaechanarsBrand
daechanarsBrand = attachment DaechanarsBrand Cards.daechanarsBrand

-- | Attached hero gets +1 attack (+2 attack instead when attacking an Undead enemy).
instance HasModifiersFor DaechanarsBrand where
  getModifiersFor (DaechanarsBrand a) = case a.placement of
    AttachedTo host -> modified a host [AttackModifier 1, AttackBonusAgainst Undead 1]
    _ -> pure ()

-- | Response: After you play Daechanar's Brand, search the top 5 cards of the encounter
-- deck for a Sorcery treachery and add it to the victory display. Shuffle the encounter
-- deck.
instance HasAbilities DaechanarsBrand where
  getAbilities (DaechanarsBrand a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.cardId)]

instance RunMessage DaechanarsBrand where
  runMessage msg s@(DaechanarsBrand a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let sorceries = [c | c <- take 5 deck, cdCardType (toCardDef c) == TreacheryType, Sorcery `elem` cdCardTraits (toCardDef c)]
      chooseOrRunOneM pid do
        questionLabeled "daechanarsBrand.choose"
        causedBy a
        showing (map EncounterCard sorceries)
        targets [CardIdTarget c.id | c <- sorceries] addToVictoryDisplay
      shuffleEncounterDeck
      pure s
    _ -> DaechanarsBrand <$> runMessage msg a
