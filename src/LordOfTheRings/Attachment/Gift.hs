-- | What The Road Darkens' Gift boons, and Palantír of Orthanc, share as their cost.
module LordOfTheRings.Attachment.Gift (spendBoon) where

import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Campaign (CampaignLogUpdate (RemoveFromPool))
import LordOfTheRings.Card (toCardCode)

{- | "Add it to the victory display and remove it from the campaign pool", as a cost paid as
the effect resolves. The removal is written to the scenario's campaign log, kept only if the
scenario is won (The Road Darkens rules insert, FAQ on Leaf-wrapped Lembas).
-}
spendBoon :: ReverseQueue m => AttachmentAttrs -> m ()
spendBoon a = do
  addToVictoryDisplay a
  updateCampaignLog $ RemoveFromPool (toCardCode a)
