module LordOfTheRings.Treachery.Cards.MisplacedTrust (misplacedTrust) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MisplacedTrust = MisplacedTrust TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden of the Ered Mithrin campaign, from Roam Across Rhovanion, with a player card
back: it functions like a player card ('FunctionsLikeAPlayerCard'). Its Setup, into its
player's deck after the starting hand is drawn, is the campaign pool's (a pool card with a
seat).
-}
misplacedTrust :: TreacheryCard MisplacedTrust
misplacedTrust = treachery MisplacedTrust Cards.misplacedTrust

-- | Counts as a Condition attachment with the text: "Attached character cannot have player
-- card attachments and is immune to player card effects."
instance HasModifiersFor MisplacedTrust where
  getModifiersFor (MisplacedTrust a) = case a.placement of
    AttachedTo host -> modified a host [CannotHavePlayerAttachments, ImmuneToPlayerCardEffects]
    _ -> pure ()

-- | Forced: After you draw this card, attach it to a hero you control. (Its card, out of
-- play in the hand, answers the draw.)
instance RunMessage MisplacedTrust where
  runMessage msg t@(MisplacedTrust attrs) = case msg of
    EncounterCardDrawn pid card | card.id == attrs.cardId -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "misplacedTrust.hero"
        showing [EncounterCard card]
        targets heroes \h -> attachEncounterCardFromHand pid card h
      pure t
    _ -> MisplacedTrust <$> runMessage msg attrs
