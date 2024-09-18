// From LeetCode #31
// Implements https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
//
// I've had issues with permutations for a while and this
// is a clean algorithm for creating permutations.
class Solution {
    public void nextPermutation(int[] nums) {
        int pivot = -1;
        for (int k = 0; k < nums.length - 1; k++) {
            if (nums[k] < nums[k + 1]) {
                pivot = k;
            }
        }

        if (pivot >= 0) {
            int swap = pivot + 1;
            for (int l = pivot + 2; l < nums.length; l++ ) {
                if (nums[l] > nums[pivot]) {
                    swap = l;
                }
            }

            int tmp = nums[pivot];
            nums[pivot] = nums[swap];
            nums[swap] = tmp;
        }

        // reverse everything after the pivot
        int temp;
        for (int i = pivot + 1; i < nums.length - (nums.length - pivot)/2; i++) {
            int replacementSpot = nums.length - (i - pivot);
            temp = nums[i];
            nums[i] = nums[replacementSpot];
            nums[replacementSpot] = temp;
        }
    }
}
