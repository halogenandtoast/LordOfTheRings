module LordOfTheRings.Objective.Cards.OnTrack (offTrack, onTrack) where

import LordOfTheRings.Card (CardType (LocationType), cdCardType, cdKeywords, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Track))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (onTrackMark)

-- | The Search objective of Beneath the Sands: one card, Off Track on one side and On Track
-- on the other, the side faceup kept as its card code.
newtype OnTrack = OnTrack ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

offTrack :: ObjectiveCard OnTrack
offTrack = objective OnTrack Cards.offTrack

onTrack :: ObjectiveCard OnTrack
onTrack = objective OnTrack Cards.onTrack

isOn :: ObjectiveAttrs -> Bool
isOn a = toCardCode a == toCardCode Cards.onTrack

-- | Off Track: Progress cannot be placed on the main quest.
instance HasModifiersFor OnTrack where
  getModifiersFor (OnTrack a) = unless (isOn a) do
    mains <- getMainQuests
    for_ mains \q -> modified a q [ProgressLimit 0]

{- | On Track: Forced: At the end of the quest phase, search the top 3 cards of the
encounter deck for a card with On Track, add it to the staging area and discard the rest.
If no card enters play from this effect, flip this objective. Forced: After the players
quest unsuccessfully, flip this objective.

Off Track: Forced: At the end of the quest phase, place 1 resource here. Then, either
return the topmost location in the encounter discard pile to the staging area, or raise
each player's threat by 1 for each resource here. And the Track keyword: after the players
travel to a location while Off Track, they discard X cards from the encounter deck, X its
Track value, and flip to On Track if one of them has On Track printed (rule sheet). The
deck reset partway through goes on discarding, as 'DiscardTopOfEncounterDeck' does.
-}
instance HasAbilities OnTrack where
  getAbilities (OnTrack a)
    | isOn a =
        [ mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase
        , mkAbility a 2 $ forced $ QuestedUnsuccessfullyWindow After
        ]
    | otherwise =
        [ mkAbility a 3 $ forced $ PhaseEndsWindow When QuestPhase
        , mkAbility a 4 $ silent $ AnyTraveledWindow After
        ]

instance RunMessage OnTrack where
  runMessage msg o@(OnTrack attrs) = case msg of
    -- the rest of the top 3 are discarded as the one found is added: the same cards end
    -- where they would
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 3
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      flipOver attrs
      pure o
    UseCardAbility _ (isSource attrs -> True) 3 _ -> do
      placeResources attrs attrs 1
      locations <- filter ((== LocationType) . cdCardType . toCardDef) <$> getEncounterDiscard
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "offTrack.choose"
        causedBy attrs
        targets (take 1 locations) addToStagingFromDiscard
        labeled "offTrack.raiseThreat" $ eachPlayer \p -> encounterRaisesThreat p (attrs.resources + 1)
      pure o
    -- Lost Underground's "the Search objective cannot flip" stops the Track keyword too
    UseCardAbility _ (isSource attrs -> True) 4 _ -> do
      barred <- elem CannotBeFlipped <$> getModifiers attrs
      locations <- getsGame (filter ((== ActiveLocation) . (.placement)) . toList . (.entities.locations))
      let x = sum [n | l <- locations, Track n <- toList (cdKeywords $ toCardDef l)]
      unless (barred || x <= 0) $ discardTopOfEncounterDeck attrs x
      pure o
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards
      | isOn attrs -> do
          let found = filter (cardMatches onTrackMark) cards
          if null found
            then flipOver attrs
            else do
              lead <- getLead
              chooseOrRunOneM lead do
                questionLabeled "onTrack.add"
                causedBy attrs
                targets found addToStagingFromDiscard
          pure o
      | otherwise -> do
          when (any (cardMatches onTrackMark) cards) $ flipOver attrs
          pure o
    -- turned from Off Track to On Track, its resources are discarded (rule sheet)
    Flip (isTarget attrs -> True) -> do
      let side = if isOn attrs then Cards.offTrack else Cards.onTrack
      push $ CheckWindows [mkAfter $ FlippedTo (toTarget attrs) (toCardCode side)]
      pure $ OnTrack $ attrs {objectiveCardCode = toCardCode side, objectiveTokens = mempty}
    _ -> OnTrack <$> runMessage msg attrs
