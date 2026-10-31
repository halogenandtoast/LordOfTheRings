module LordOfTheRings.Treachery.Cards.OminousFate (ominousFate) where

import LordOfTheRings.Campaign (CampaignLogKey (ChosenDuringSetup), recorded)
import LordOfTheRings.Helpers.Campaign (campaignPartNumber, getCampaignLog)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OminousFate = OminousFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Dream-chaser campaign.
ominousFate :: TreacheryCard OminousFate
ominousFate = treachery OminousFate Cards.ominousFate

-- | Surge. Ominous Fate cannot be canceled.
instance HasModifiersFor OminousFate where
  getModifiersFor (OminousFate a) = modifySelf a [WhenRevealedCannotBeCanceled]

{- | When Revealed: Each player must either reveal an encounter card, raise their threat by
X, or deal X damage divided among characters they control. X is the campaign card's part
number (plus 1 for each boon a setup had the players choose, as Raid on the Grey Havens').
The players may add Ominous Fate to the victory display.
Shadow: Reveal an encounter card.
-}
instance RunMessage OminousFate where
  runMessage msg t@(OminousFate attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      part <- campaignPartNumber
      chosen <- maybe 0 (length . recorded ChosenDuringSetup) <$> getCampaignLog
      let x = max 1 (part + chosen)
      eachPlayer \pid -> chooseOneM pid do
        questionLabeledWith "ominousFate.choose" [intVar "x" x]
        causedBy attrs
        labeled "ominousFate.reveal" revealEncounterCard
        labeledWith "ominousFate.threat" [intVar "x" x] $ raiseThreat pid x
        labeledWith "ominousFate.damage" [intVar "x" x] $ dealDamageDivided attrs pid x
      pure t
    -- once resolved and discarded, it may go to the victory display
    Discard (isTarget attrs -> True) -> do
      t' <- OminousFate <$> runMessage msg attrs
      lead <- getLead
      chooseOneM lead do
        questionLabeled "ominousFate.victory"
        causedBy attrs
        labeled "ominousFate.addToVictory" $ addToVictoryDisplay (CardIdTarget attrs.cardId)
        labeled "ominousFate.leave" $ pure ()
      pure t'
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      revealEncounterCard
      pure t
    _ -> OminousFate <$> runMessage msg attrs
